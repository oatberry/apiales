(local riscv (require :compiler.riscv))
(local analysis (require :compiler.analysis))
(local {: errorf : assertf : extend-list} (require :compiler.util))

;; Type pretty printing
(fn type->string [t]
  (match t
    [:simple-type u] u
    [:pointer-type u] (.. (type->string u) "*")
    _ (tostring t)))

;; Codeblocks
(fn code->string [code]
  (let [lines (icollect [_ inst (ipairs code)]
                (tostring inst))]
    (table.concat lines "\n")))

(local code-mt {:__tostring code->string})

;; .asm contents
(fn asm->string [{: symbol-table : code}]
  (let [format-lines [".section .text"
                      "%s"
                      ""
                      ""
                      ".section .strings"
                      "%s"]
        format-str (table.concat format-lines "\n")
        strings-section (icollect [_ sym-entry (pairs symbol-table.syms)]
                          (match sym-entry.sym-type
                            [:simple-type :string]
                            (: "0x%08x \"%s\"" :format sym-entry.address sym-entry.init)))]
    (format-str:format code (table.concat strings-section "\n"))))

(local asm-mt {:__tostring asm->string})

(fn mk-counter [prefix]
  {:value 0
   :next (fn [self]
           (set self.value (+ self.value 1))
           (if prefix
               (.. prefix self.value)
               self.value))
   :reset (fn [self]
            (set self.value 0))})

(local label-mt {:__tostring (fn [{: label}]
                               (.. label ":"))})

(fn label [name]
  (setmetatable {:label name}
                label-mt))

(fn mk-label-generator [...]
  (let [prefixes [...]
        counter (mk-counter)]
    #(let [count (counter:next)]
       (table.unpack (icollect [_ prefix (ipairs prefixes)]
                       (label (.. prefix "_" count)))))))

(fn info [{: temp : val-type : lval : local? : offset}]
  {:lval (or lval false)
   : val-type
   : temp
   : local?
   : offset})

;; Code generation
(fn ast->code [analyzed-ast lens]
  (local int-reg-counter (mk-counter "t"))
  (local float-reg-counter (mk-counter "f"))

  (local gen-if (mk-label-generator "if_out"))
  (local gen-if-else (mk-label-generator "else" "ifelse_out"))
  (local gen-while (mk-label-generator "loop" "loop_out"))

  (var current-return-label nil)

  (fn gen-temp [val-type]
    (match val-type
      [:simple-type :float] (float-reg-counter:next)
      [:simple-type :int] (int-reg-counter:next)
      [:simple-type :void] (int-reg-counter:next)
      [:pointer-type _] (int-reg-counter:next)
      _ (errorf "Unhandled type for register generation: %s" (type->string val-type))))

  (local code (setmetatable [] code-mt))
  (fn emit [...]
    (each [_ v (ipairs [...])]
      (table.insert code v)))

  (fn branch [operator lhs-temp rhs-temp val-type label]
    (let [(branch-op ?float-comp-op) (riscv.cond->op operator val-type)]
      (match val-type
        [:simple-type :float] (let [cond (int-reg-counter:next)]
                                (emit (?float-comp-op cond lhs-temp rhs-temp)
                                      (branch-op cond "x0" label.label)))
        _ (emit (branch-op lhs-temp rhs-temp label.label)))))

  (fn rvalify [expr]
    (let [load-op (match expr.val-type
                    [:simple-type :int] riscv.lw
                    [:simple-type :float] riscv.flw
                    [:pointer-type _] riscv.lw
                    _ (errorf "Compile error: cannot rvalify expr of type '%s'"
                              (type->string expr.val-type)))
          dest (gen-temp expr.val-type)]
      (if expr.local?
          (emit (load-op dest expr.offset :fp))
          (emit (load-op dest 0 expr.temp)))
      (info {:temp dest :val-type expr.val-type})))

  (fn ensure-rval [expr]
    (if expr.lval (rvalify expr) expr))

  (local go {})

  (fn go.dispatch [node]
    (match node
      [node-type & fields]
      (let [func (assertf (. go node-type)
                          "Missing codegen for node type `%s`" node-type)]
        (func (setmetatable fields (getmetatable node))))))

  (fn go.each [statements]
    (each [_ statement (ipairs statements)]
      (go.dispatch statement)))

  (fn go.program [[decls functions &as node]]
    (emit (riscv.mv :fp :sp)
          (riscv.jr "func_main")
          (riscv.halt))
    (each [_ func (ipairs functions)]
      (emit "")
      (go.dispatch func)))

  (fn go.function [[return-type [_ id] params decls statements &as node]]
    (int-reg-counter:reset)
    (float-reg-counter:reset)
    (set current-return-label (.. "func_ret_" id))

    (emit (label (.. "func_" id))  ; function label
          (riscv.sw :fp 0 :sp)     ; save frame pointer
          (riscv.mv :fp :sp)       ; set new frame pointer
          (riscv.addi :sp :sp -4)  ; incr stack
          (riscv.addi :sp :sp (+ node.scope.locals-counter.value 4)))

    (local reg-saving-code (setmetatable [] code-mt))
    (emit reg-saving-code)
    (go.each statements)

    (for [t 1 int-reg-counter.value]
      (table.insert reg-saving-code (riscv.sw (.. "t" t) 0 :sp))
      (table.insert reg-saving-code (riscv.addi :sp :sp -4)))
    (for [f 1 float-reg-counter.value]
      (table.insert reg-saving-code (riscv.fsw (.. "f" f) 0 :sp))
      (table.insert reg-saving-code (riscv.addi :sp :sp -4)))

    (emit (label current-return-label))
    (for [f float-reg-counter.value 1 -1]
      (emit (riscv.addi :sp :sp 4)
            (riscv.flw (.. "f" f) 0 :sp)))
    (for [t int-reg-counter.value 1 -1]
      (emit (riscv.addi :sp :sp 4)
            (riscv.lw (.. "t" t) 0 :sp)))
    (emit (riscv.mv :sp :fp)
          (riscv.lw :fp 0 :fp)
          (riscv.ret)))

  (fn go.read-stmt [[id &as node]]
    (assertf id.sym-entry
             "Compile error: %s: read(): argument must be a variable"
             id.source)
    (let [variable (go.dispatch id)
          [read-op store-op] (match variable.val-type
                               [:simple-type :int] [riscv.geti riscv.sw]
                               [:simple-type :float] [riscv.getf riscv.fsw]
                               _ (errorf "Compile error: %s: read(): cannot read into variable of type '%s'"
                                         node.source (type->string variable.val-type)))
          dest (gen-temp variable.val-type)]
      (emit (read-op dest)
            (if variable.local?
                (store-op dest variable.offset :fp)
                (store-op dest 0 variable.temp)))))

  (fn go.print-stmt [[expr &as node]]
    (let [expr (go.dispatch expr)]
      (match expr.val-type
        [:simple-type :string] (emit (riscv.puts expr.temp))
        _ (let [expr (ensure-rval expr)
                put-op (match expr.val-type
                         [:simple-type :int] riscv.puti
                         [:simple-type :float] riscv.putf
                         _ (errorf "Compile error: %s: print(): cannot print expr of type '%s'"
                                   node.source (type->string expr.val-type)))]
            (emit (put-op expr.temp))))))

  (fn go.return-stmt [[?expr &as node]]
    (when ?expr
      (let [expr (ensure-rval (go.dispatch ?expr))
            store-op (riscv.type->store expr.val-type)]
        (emit (store-op expr.temp 8 :fp))))
    (emit (riscv.j current-return-label)))

  (fn go.func-call [[[_ func-name &as id] args &as node]]
    (assertf (= :function id.sym-entry.type)
             "Compile error: %s: cannot call non-function symbol." node.source)
    (local is-void? (= :void (. id.sym-entry.return-type 2)))

    (each [_ arg (ipairs args)]
      (let [value (ensure-rval (go.dispatch arg))
            store-op (riscv.type->store value.val-type)]
        (emit (store-op value.temp 0 :sp)
              (riscv.addi :sp :sp -4))))

    (let [load-op (riscv.type->load id.sym-entry.return-type)
          return-reg (or is-void? (gen-temp id.sym-entry.return-type))]
      (emit (riscv.addi :sp :sp -4)
            (riscv.sw :ra 0 :sp)
            (riscv.addi :sp :sp -4)
            (riscv.jr (.. "func_" func-name))
            (riscv.addi :sp :sp 4)
            (riscv.lw :ra 0 :sp)
            (riscv.addi :sp :sp 4))
      (when (not is-void?)
        (emit (load-op return-reg 0 :sp)))
      (emit (riscv.addi :sp :sp (* 4 (length id.sym-entry.arg-types))))
      (info {:temp return-reg :val-type id.sym-entry.return-type})))

  (fn go.malloc [[expr &as node]]
    (let [expr (ensure-rval (go.dispatch expr))
          dest (int-reg-counter:next)]
      (emit (riscv.malloc dest expr.temp))
      (info {:temp dest :val-type [:pointer-type [:simple-type :void]]})))

  (fn go.free [[expr &as node]]
    (let [expr (ensure-rval (go.dispatch expr))]
      (emit (riscv.free expr.temp))))

  (fn go.assign-stmt [[lhs expr &as node]]
    (let [lhs (go.dispatch lhs)
          rhs (ensure-rval (go.dispatch expr))
          store-op (riscv.type->store lhs.val-type)]
      (assertf lhs.lval "Compile error: %s: assignment lhs is not an lvalue" node.source)
      (if lhs.local?
          (emit (store-op rhs.temp lhs.offset :fp))
          (emit (store-op rhs.temp 0 lhs.temp)))))

  (fn go.deref [[expr &as node]]
    (let [expr (ensure-rval (go.dispatch expr))
          [_ val-type] expr.val-type]
      (info {:lval true :local? false :temp expr.temp
             : val-type})))

  (fn go.addr-of [[expr &as node]]
    (let [expr (go.dispatch expr)]
      (assert expr.lval)
      (if expr.local?
          (let [dest (int-reg-counter:next)]
            (emit (riscv.addi dest :fp expr.offset))
            (info {:temp dest :val-type [:pointer-type expr.val-type]}))
          (info {:temp expr.temp :val-type [:pointer-type expr.val-type]}))))

  (fn go.cast-expr [[cast-type expr &as node]]
    (let [expr (go.dispatch expr)]
      (set expr.val-type cast-type)
      expr))

  (fn go.while-stmt [[cond statements &as node]]
    (let [(loop-lbl out-lbl) (gen-while)]
      (emit loop-lbl)
      (let [[_ operator l-expr r-expr] cond
            lhs (ensure-rval (go.dispatch l-expr))
            rhs (ensure-rval (go.dispatch r-expr))]
        (branch operator lhs.temp rhs.temp lhs.val-type out-lbl)
        (go.each statements)
        (emit (riscv.j loop-lbl.label)
              out-lbl))))

  (fn go.if-stmt [[cond statements ?else-statements &as node]]
    (let [[_ operator l-expr r-expr] cond
          lhs (ensure-rval (go.dispatch l-expr))
          rhs (ensure-rval (go.dispatch r-expr))]
      (if ?else-statements
          (let [(else-lbl out-lbl) (gen-if-else)]
            (branch operator lhs.temp rhs.temp lhs.val-type else-lbl)
            (go.each statements)
            (emit (riscv.j out-lbl.label)
                  else-lbl)
            (go.each ?else-statements)
            (emit out-lbl))
          (let [out-lbl (gen-if)]
            (branch operator lhs.temp rhs.temp lhs.val-type out-lbl)
            (go.each statements)
            (emit out-lbl)))))

  (fn go.binary-op [[operator l-expr r-expr &as node]]
    (let [lhs (ensure-rval (go.dispatch l-expr))
          rhs (ensure-rval (go.dispatch r-expr))
          math-op (riscv.operator->op operator lhs.val-type)
          dest (gen-temp lhs.val-type)]
      (emit (math-op dest lhs.temp rhs.temp))
      (info {:temp dest :val-type lhs.val-type})))

  (fn go.unary-op [[op expr &as node]]
    (let [value (ensure-rval (go.dispatch expr))
          op (riscv.operator->op "-" value.val-type :unary)
          dest (gen-temp value.val-type)]
      (emit (op dest value.temp))
      (info {:temp dest :val-type value.val-type})))

  (fn go.identifier [[symbol &as node]]
    (assertf node.sym-entry "Complier error: %s: identifier node has no symbol entry"
             node.source)
    (if node.sym-entry.local?
        (info {:local? true :lval true :offset node.sym-entry.address
               :val-type node.sym-entry.sym-type})
        (let [dest (int-reg-counter:next)]
          (emit (riscv.la dest node.sym-entry.address))
          (info {:temp dest :lval true
                 :val-type node.sym-entry.sym-type}))))

  (fn go.integer-lit [[integer &as node]]
    (let [dest (int-reg-counter:next)]
      (emit (riscv.li dest integer))
      (info {:temp dest :val-type [:simple-type :int]})))

  (fn go.float-lit [[float &as node]]
    (let [dest (float-reg-counter:next)]
      (emit (riscv.fimms dest float))
      (info {:temp dest :val-type [:simple-type :float]})))

  (let [top (if lens (lens analyzed-ast) analyzed-ast)]
    (go.dispatch top))

  code)

(fn generate [analyzed-program-ast symbol-table lens]
  (let [asm {:code (ast->code analyzed-program-ast lens)
             : symbol-table}]
    (setmetatable asm asm-mt)))

(fn generate-from-file [filename lens]
  (let [(ast sym-tbl) (analysis.analyze-file filename)]
    (generate ast sym-tbl lens)))

(fn generate-from-string [string lens]
  (let [(ast sym-tbl) (analysis.analyze-string string)]
    (generate ast sym-tbl lens)))

(fn test [stepnum testnum]
  (generate-from-file (: "tests/step%d/source/test%d.uC"
                         :format stepnum testnum)))

{: ast->code
 : generate
 : generate-from-file
 : generate-from-string
 : test}
