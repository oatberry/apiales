(local parsing (require :compiler.parsing))
(local symbol-table (require :compiler.symbol-table))
(local {: push : errorf : assertf} (require :compiler.util))

(fn params->types [params]
  (icollect [_ [_ param-type _] (ipairs params)]
    param-type))

(fn analyze [program-ast]
  (local logs [])
  (fn logf [level location format-str ...]
    (push logs {:msg (format-str:format ...)
                : level
                : location}))

  (local {: get-symbol-table
          : current-scope
          : add-scope
          : pop-scope
          : add-variable
          : add-function
          : add-argument
          : lookup-symbol}
         (symbol-table.mk-symbol-table))

  (local go {})

  (fn go.dispatch [node]
    (match node
      [node-type & fields]
      (let [func (assertf (. go node-type)
                          "Missing analysis for node type `%s`" node-type)]
        (func (setmetatable fields (getmetatable node))))))

  (fn go.each [statements]
    (each [_ statement (ipairs statements)]
      (go.dispatch statement)))

  (fn go.program [[decls functions &as node]]
    (go.each decls)
    (go.each functions))

  (fn go.var-decl [[base-type [_ id] &as node]]
    (add-variable id base-type nil node.source))

  (fn go.str-decl [[[_ id] init &as node]]
    (add-variable id [:simple-type :string] init node.source))

  (fn go.func-decl [[return-type [_ id] params &as node]]
    (add-function id return-type (params->types params)
                  :undefined node.source))

  (fn go.function [[return-type [_ id] params decls statements &as node]]
    (match (lookup-symbol id)
      nil (add-function id return-type (params->types params)
                        :defined node.source)
      {:type "function" :defined? false &as ste} (set ste.defined? true)
      _ (logf :error node.source "Multiple definitions for symbol: %s" id))

    (add-scope id)
    (tset (. (getmetatable node) :__index) :scope
          (current-scope))

    (for [i (length params) 1 -1]
      (match (. params i)
        ["param" arg-type ["identifier" arg-name] &as param-node]
        (add-argument arg-name arg-type param-node.source)))

    (go.each decls)
    (go.each statements)
    (pop-scope))

  (fn go.func-call [[id args &as node]]
    (go.dispatch id)
    (go.each args))

  (fn go.read-stmt [[id &as node]]
    (go.dispatch id))

  (fn go.print-stmt [[expr &as node]]
    (go.dispatch expr))

  (fn go.return-stmt [[?expr &as node]]
    (when ?expr
      (go.dispatch ?expr)))

  (fn go.assign-stmt [[id expr &as node]]
    (go.dispatch id)
    (go.dispatch expr))

  (fn go.while-stmt [[cond statements &as node]]
    (go.dispatch cond)
    (go.each statements))

  (fn go.if-stmt [[cond statements ?else-statements &as node]]
    (go.dispatch cond)
    (go.each statements)
    (when ?else-statements
      (go.each ?else-statements)))

  (fn go.cond [[op l-expr r-expr &as node]]
    (go.dispatch l-expr)
    (go.dispatch r-expr))

  (fn go.binary-op [[op l-expr r-expr &as node]]
    (go.dispatch l-expr)
    (go.dispatch r-expr))

  (fn go.unary-op [[op expr &as node]]
    (go.dispatch expr))

  (fn go.identifier [[symbol &as node]]
    (let [sym-entry (lookup-symbol symbol)
          node-mt (getmetatable node)]
      (if sym-entry
          (set node-mt.__index.sym-entry sym-entry)
          (logf error node.source "Use of undeclared variable: %s" symbol))))

  (fn go.deref [[expr &as node]]
    (go.dispatch expr))

  (fn go.addr-of [[expr &as node]]
    (go.dispatch expr))

  (fn go.cast-expr [[cast-type expr &as node]]
    (go.dispatch expr))

  (fn go.malloc [[expr &as node]]
    (go.dispatch expr))

  (fn go.free [[expr &as node]]
    (go.dispatch expr))

  (fn go.integer-lit [[integer &as node]]
    nil)

  (fn go.float-lit [[float &as node]])

  (go.dispatch program-ast)

  (when (not= 0 (length logs))
    (let [lines (icollect [_ {: msg : level : location} (ipairs logs)]
                  (: "%s: %s:\n    %s" :format location level msg))]
      (errorf "%s" (table.concat lines "\n"))))

  (values program-ast (get-symbol-table)))

(fn analyze-string [str]
  (-> str parsing.parse-string analyze))

(fn analyze-file [filename]
  (-> filename parsing.parse-file analyze))

{: analyze
 : analyze-string
 : analyze-file}
