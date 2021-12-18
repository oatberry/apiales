;;; This module implements the lexer for sillyC

(local {: errorf : lines} (require :compiler.util))

;; Tokens
(fn token-location [token]
  (if token.eof?
      "end of input"
      (: "%s:%d:%d" :format token.source-name token.line token.col)))

(fn token->string [token]
  "Stringify a token"
  (let [location (: "%d:%d" :format token.line token.col)]
    (: "(TOKEN %s:% -9s % -12s \"%s\")"
       :format token.source-name location (token.type:upper) token.lexeme)))

(fn token-source-name [token] token.source-name)
(fn token-type [token] token.type)
(fn token-lexeme [token] token.lexeme)

(local token-metatable {:__tostring token->string})

;; Lexing
(fn whitespace? [char] (char:match "%s"))
(fn digit? [char] (char:match "%d"))
(fn alpha? [char] (char:match "%a"))

(fn mk-lexer [source ?source-name]
  "Create a lexer over some source string"
  (local source-name (or ?source-name "<unknown>"))

  (var start 1)      ; start of current lexeme in source
  (var current 1)    ; end of current lexeme in source
  (var line 1)       ; current line
  (var col 1)        ; current column

  (var token-line 1) ; start line for currently processing token
  (var token-col 1)  ; start column for currently processing token

  (fn eof? [] (> current (length source)))

  (fn peek [n]
    (let [end-idx (+ current (if n (- n 1) 0))]
      (source:sub current end-idx)))

  (fn next-line []
    (set line (+ 1 line))
    (set col 1))

  (fn advance [n]
    (let [i current]
      (set current (+ current (or n 1)))
      (set col (+ col (or n 1)))
      (source:sub i (if n (+ i n) i))))

  (fn mk-token [token-type]
    (setmetatable {:type token-type
                   :source-name source-name
                   :line token-line :col token-col
                   :lexeme (source:sub start (- current 1))}
                  token-metatable))

  (fn consume [str-or-predicate]
    "When `str-or-predicate` is a function, only advance if the predicate is true over
    one character of the peek. When `str-or-pred` is a string, only advance if the
    next (length str-or-pred) bytes of peek is equivalent to `str-or-pred`"
    (match (type str-or-predicate)
      :string (let [str str-or-predicate]
                (and (not (eof?))
                     (= str (peek (length str)))
                     (advance (length str))))
      :function (let [predicate str-or-predicate]
                  (and (not (eof?))
                       (predicate (peek))
                       (advance)))))

  (fn read-while [str-or-predicate]
    (while (consume str-or-predicate)))

  (fn line-comment []
    (read-while #(not= "\n" $))
    (when (consume "\n") (next-line)))

  (fn block-comment []
    (let [start-line line
          start-col (- col 2)]
      (while (not (consume "*/"))
        (if (consume "\n") (next-line)
            (consume "/*") (block-comment)
            (eof?) (errorf "%s:%d:%d: Unterminated block comment."
                           source-name start-line start-col)
            (advance)))))

  (fn skip-whitespace []
    (if (consume "\n") (do (next-line) (skip-whitespace))
        (consume "//") (do (line-comment) (skip-whitespace))
        (consume "/*") (do (block-comment) (skip-whitespace))
        (consume whitespace?) (skip-whitespace)))

  (fn scan-number []
    (read-while digit?)
    (when (consume ".")
      (when (not (digit? (peek)))
        (errorf "%s:%d:%d: Invalid float literal"
                source-name token-line token-col))
      (read-while digit?))
    (if (alpha? (peek))
        (errorf "%s:%d:%d: Identifier cannot start with a number."
                source-name token-line token-col)
        (mk-token :number)))

  (fn scan-string []
    (read-while #(string.match $ "[^\n\"\\]"))

    (if (consume "\n") (do (next-line)
                           (scan-string))
        (consume "\\") (do (advance) ; escaped char
                           (scan-string))
        (consume "\"") (mk-token :string)
        (errorf "Unterminated string starting at %s:%d:%d"
                source-name token-line token-col)))

  (fn scan-symbol []
    (read-while #(or (digit? $) (alpha? $) (= "_" $)))
    (mk-token :symbol))

  (fn scan-token []
    (skip-whitespace)

    (set start current)
    (set token-line line)
    (set token-col col)

    (match (advance)
      "(" (mk-token "(")
      ")" (mk-token ")")
      "{" (mk-token "{")
      "}" (mk-token "}")
      "[" (mk-token "[")
      "]" (mk-token "]")
      ";" (mk-token ";")
      "," (mk-token ",")
      "+" (mk-token "+")
      "-" (mk-token "-")
      "*" (mk-token "*")
      "&" (mk-token "&")
      "/" (mk-token "/")
      "!" (mk-token (if (consume "=") "!=" "!"))
      "=" (mk-token (if (consume "=") "==" "="))
      "<" (mk-token (if (consume "=") "<=" "<"))
      ">" (mk-token (if (consume "=") ">=" ">"))
      "\"" (scan-string)
      "" nil
      char (if (or (= "_" char) (alpha? char)) (scan-symbol)
               (digit? char) (scan-number)
               (mk-token :unknown))))

  scan-token)

(lambda scan-string [source ?source-name]
  (values (icollect [token (mk-lexer source ?source-name)]
            token)
          (lines source)))

(lambda scan-file [filename]
  (with-open [file (assert (io.open filename))]
    (scan-string (file:read "a") filename)))

(lambda pp-file-tokens [filename]
  (icollect [_ token (ipairs (scan-file filename))]
    [(token-location token) (token-type token) (token-lexeme token)]))

;;; Module exports
{: mk-lexer
 : scan-string
 : scan-file
 : pp-file-tokens

 : token->string
 : token-type
 : token-location
 : token-lexeme}
