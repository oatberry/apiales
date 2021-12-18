;;; This module implements the parser for sillyC.

(local util (require :compiler.util))
(local ast (require :compiler.ast))
(local lexing (require :compiler.lexing))

(local {: token-type : token-location : token-lexeme}
       lexing)

(fn mk-parser [tokens lines]
  "Create a parser over some source tokens."
  (var current-index 1) ; current token index

  (fn parse-errorf [token msg ...]
    (util.errorf "Parse error: %s\n  --> %s\n% 2d | %s"
                 (msg:format ...)
                 (token-location token)
                 token.line (. lines token.line)))

  (fn eof-errorf [msg ...]
    (parse-errorf {:eof? true :line (length lines)}
                  (or msg "Unexpected end of input") ...))

  (fn eof? [] (> current-index (length tokens)))

  (fn peek [n?]
    "Look at the next `n`th token without consuming it."
    (when (eof?)
      (eof-errorf))
    (let [i (+ current-index (if n? (- n? 1) 0))]
      (. tokens i)))

  (fn advance []
    "Consume and return the next token."
    (when (eof?)
      (eof-errorf))
    (let [token (. tokens current-index)]
      (set current-index (+ current-index 1))
      token))

  (fn consume [...]
    "Consume the next token only if it matches any of the provided token types."
    (let [types (util.list->set [...])
          token-matches? (and (not (eof?))
                              (->> (peek) (token-type) (. types)))]
      (if token-matches? (advance) nil)))

  (fn consume-symbol [...]
    "Consume the next token only if it is one of the provided symbols."
    (if (eof?) nil

        (let [lexemes (util.list->set [...])
              token-matches? (->> (peek) (token-lexeme) (. lexemes))]
          (if token-matches? (advance) nil))))

  (fn expect [...]
    (or (consume ...)
        (parse-errorf (peek) "Expected a token one of [%s], but got: %s"
                      (table.concat [...] ", ") (token-type (peek)))))

  (fn expect-symbol [...]
    (or (consume-symbol ...)
        (parse-errorf (peek) "Expected a symbol one of [%s], but got: %s"
                      (table.concat [...] ", ") (token-lexeme (peek)))))

  (fn maybe [parser]
    (let [saved-pos current-index
          (ok? result) (pcall parser)]
      (when (not ok?)
        (set current-index saved-pos))
      (if ok?
          result
          (values nil result))))

  (fn many-ending-with [p end]
    (local items [])
    (fn go []
      (match (maybe p)
        item (do (table.insert items item)
                 (go))
        (nil err) (if (= end (token-lexeme (peek)))
                      items
                      (error err))))
    (go))

  (fn alt [...]
    (local parsers [...])
    (fn go [n]
      (when (<= n (length parsers))
        (let [result (maybe (. parsers n))]
          (or result (go (+ n 1))))))
    (go 1))

  (fn parse-wrapped [open-delim p close-delim]
    (let [open-token (expect open-delim)
          items [(p)]]
      (when (eof?) (eof-errorf "Unclosed delimiter starting at %s"
                               (token-location open-token)))
      (when (not= close-delim (token-type (peek)))
        (parse-errorf (peek) "Unclosed delimiter starting at %s"
                      (token-location open-token)))
      (expect close-delim)
      (table.unpack items)))

  ;; Putting parsing functions in a table enables them to be mutually recursive.
  (local parser {})

  (fn parser.program []
    (ast.program (peek) (parser.decls) (parser.functions)))

  (fn parser.decls []
    (icollect [decl #(alt parser.var-decl parser.str-decl parser.func-decl)]
      decl))

  (fn parser.var-decl []
    (ast.var-decl (peek) (parser.type) (parser.identifier)
                  (expect ";")))

  (fn parser.str-decl []
    (let [start-token (peek)
          _ (expect-symbol "string")
          identifier (parser.identifier)
          _ (expect "=")
          init (parser.string)
          _ (expect ";")]
      (ast.str-decl start-token identifier init)))

  (fn parser.func-decl []
    (ast.func-decl (peek)
                   (parser.type)
                   (parser.identifier)
                   (parse-wrapped "(" parser.params ")")
                   (expect ";")))

  (fn parser.params []
    (match (maybe parser.param)
      first (icollect [param #(when (consume ",") (parser.param))
                       :into [first]]
              param)
      nil []))

  (fn parser.param []
    (ast.param (peek) (parser.type) (parser.identifier)))

  (fn parser.simple-type []
    (let [token (expect-symbol :int :float :void :string)]
      (ast.simple-type token (token-lexeme token))))

  (fn parser.type []
    (accumulate [left (parser.simple-type)
                 pointer #(consume "*")]
      (ast.pointer-type pointer left)))

  (fn parser.functions []
    (icollect [function #(when (not (eof?)) (parser.function))]
      function))

  (fn parser.function []
    (ast.function (peek)
                  (parser.type)
                  (parser.identifier)
                  (parse-wrapped "(" parser.params ")")
                  (parse-wrapped "{" parser.fn-body "}")))

  (fn parser.fn-body []
    (values (icollect [decl #(maybe parser.var-decl)] decl)
            (parser.statements)))

  (fn parser.statements []
    (many-ending-with parser.statement "}"))

  (fn parser.statement []
    (match (token-lexeme (peek))
      "if" (parser.if-statement)
      "while" (parser.while-statement)
      _ (let [base-statement (parser.base-statement)
              _ (expect ";")]
          base-statement)))

  (fn parser.if-statement []
    (ast.if-stmt (expect-symbol "if")
                 (parse-wrapped "(" parser.condition ")")
                 (parse-wrapped "{" parser.statements "}")
                 (when (consume-symbol "else")
                   (parse-wrapped "{" parser.statements "}"))))

  (fn parser.while-statement []
    (ast.while-stmt (expect-symbol "while")
                    (parse-wrapped "(" parser.condition ")")
                    (parse-wrapped "{" parser.statements "}")))

  (fn parser.base-statement []
    (match (token-lexeme (peek))
      "read" (parser.read-statement)
      "print" (parser.print-statement)
      "return" (parser.return-statement)
      _ (or (maybe parser.func-call)
            (parser.assign-statement))))

  (fn parser.read-statement []
    (ast.read-stmt (expect-symbol "read")
                   (parse-wrapped "(" parser.identifier ")")))

  (fn parser.print-statement []
    (ast.print-stmt (expect-symbol "print")
                    (parse-wrapped "(" parser.expression ")")))

  (fn parser.return-statement []
    (ast.return-stmt (expect-symbol "return")
                     (match (maybe parser.expression)
                       expr expr
                       (nil err) (if (= ";" (token-lexeme (peek)))
                                     nil
                                     (error err)))))

  (fn parser.assign-statement []
    (let [start-token (peek)
          lhs (parser.lval)
          _ (expect "=")
          expr (parser.expression)]
      (ast.assign-stmt start-token lhs expr)))

  (fn parser.ptr-expr []
    (let [start-token (expect "*")
          expr (parser.primary-expr)]
      (ast.deref start-token expr)))

  (fn parser.addr-of-expr []
    (let [start-token (expect "&")
          expr (parser.lval)]
      (ast.addr-of start-token expr)))

  (fn parser.index []
    (let [start-token (expect "[")
          expr (parser.expression)
          _ (expect "]")]
      [start-token expr]))

  (fn parser.simple-lval []
    (match (token-type (peek))
      "*" (parser.ptr-expr)
      "symbol" (parser.identifier)
      other (parse-errorf (peek) "Unexpected token while parsing lval: %s"
                          other)))
  (fn parser.lval []
    (fn index-expr []
      [(peek) (parse-wrapped "[" parser.expression "]")])

    (accumulate [lhs (parser.simple-lval)
                 [start-token rhs] #(maybe index-expr)]
      (->> rhs
           (ast.binary-op start-token "*" (ast.integer-lit start-token 4))
           (ast.binary-op start-token "+" lhs)
           (ast.deref start-token))))

  (fn parser.condition []
    (let [lhs (parser.expression)
          token (expect "<=" "<" ">=" ">" "==" "!=")
          rhs (parser.expression)]
      (ast.cond token (token-lexeme token) lhs rhs)))

  (fn parser.expression []
    (accumulate [lhs (parser.term-expr)
                 token #(consume "+" "-")]
      (ast.binary-op token (token-lexeme token) lhs (parser.term-expr))))

  (fn parser.term-expr []
    (accumulate [lhs (parser.primary-expr)
                 token #(consume "*" "/")]
      (ast.binary-op token (token-lexeme token) lhs (parser.primary-expr))))

  (fn parser.primary-expr []
    (match (token-type (peek))
      :symbol (match (token-type (peek 2))
                "(" (parser.func-call)
                _ (parser.lval))
      :number (parser.number)
      "*" (parser.ptr-expr)
      "&" (parser.addr-of-expr)
      "(" (or (maybe parser.cast-expr)
              (parse-wrapped "(" parser.expression ")"))
      "-" (parser.prefix-op)
      _ (parse-errorf (peek) "Unexpected token while trying to parse expression: %s"
                      (token-lexeme (peek)))))

  (fn parser.cast-expr []
    (let [start-token (expect "(")
          cast-type (parser.type)
          _ (expect ")")
          expr (parser.primary-expr)]
      (ast.cast-expr start-token cast-type expr)))

  (fn parser.func-call []
    (match (token-lexeme (peek))
      "free" (ast.free (expect-symbol "free")
                       (parse-wrapped "(" parser.expression ")"))
      "malloc" (ast.malloc (expect-symbol "malloc")
                           (parse-wrapped "(" parser.expression ")"))
      _ (ast.func-call (peek)
                       (parser.identifier)
                       (parse-wrapped "(" parser.arg-list ")"))))

  (fn parser.arg-list []
    (match (maybe parser.expression)
      first (icollect [expr #(when (consume ",") (parser.expression))
                       :into [first]]
              expr)
      nil []))

  (fn parser.identifier []
    (let [token (expect :symbol)]
      (ast.identifier token (token-lexeme token))))

  (fn parser.number []
    (let [token (expect :number)
          number (tonumber (token-lexeme token))]
      (match (math.type number)
        :integer (ast.integer-lit token number)
        :float (ast.float-lit token number)
        nil (parse-errorf token "Could not parse `%s` as a number"
                          (token-lexeme token)))))

  (fn parser.string []
    (-> (expect :string)
        token-lexeme
        (string.sub 2 -2)))

  (fn parser.prefix-op []
    (let [op-token (expect "-")
          expr (parser.expression)]
      (ast.unary-op op-token "-" expr)))

  (fn parser.all-input-consumed? []
    (eof?))

  parser)

(fn parse-tokens [tokens lines ?subparser-name]
  (let [parser (mk-parser tokens lines)
        subparser (assert (. parser (or ?subparser-name :program))
                          "No such sub-parser")
        result (subparser)]
    (assert (parser.all-input-consumed?) "Parser didn't consume all input")
    result))

(fn parse-string [string ?source-name ?subparser-name]
  (let [(tokens lines) (lexing.scan-string string ?source-name)]
    (parse-tokens tokens lines ?subparser-name)))

(fn parse-file [filename ?subparser-name]
  (with-open [file (assert (io.open filename))]
    (parse-string (file:read "a") filename ?subparser-name)))

;;; Module exports
{: mk-parser
 : parse-tokens
 : parse-string
 : parse-file}
