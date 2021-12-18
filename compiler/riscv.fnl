(local op3 "%s %s, %s, %s")
(local op2 "%s %s, %s")
(local op1 "%s %s")
(local ls  "%s %s, %s(%s)")
(local put "%s %s")
(local get "%s %s")
(local imm "%s %s, %s")
(local branch "%s %s, %s, %s")

(local op-defs
       {:li     imm
        :la     "%s %s, 0x%08x"
        :add    op3
        :addi   op3
        :sub    op3
        :div    op3
        :mul    op3
        :neg    op2
        :mv     op2
        :lw     ls
        :sw     ls
        :puts   put
        :puti   put
        :geti   op1
        :ret    "%s"
        :halt   "%s"
        :fadd.s op3
        :fsub.s op3
        :fdiv.s op3
        :fmul.s op3
        :fmv.s  op2
        :fneg.s op2
        :flw    ls
        :fsw    ls
        :getf   get
        :putf   put
        :fimm.s imm
        :j      op1
        :jr     op1
        :blt    branch
        :ble    branch
        :bgt    branch
        :bge    branch
        :beq    branch
        :bne    branch
        :flt.s  op3
        :fle.s  op3
        :feq.s  op3

        ;; FAAAAKE
        :malloc op2
        :free   op1})

(local riscv
       (collect [op format-str (pairs op-defs)]
         (values (op:gsub "%." "")
                 #(format-str:format (op:upper) $...))))

(fn riscv.operator->op [operator val-type unary?]
  (let [simple-type (match val-type
                      [:simple-type :int] :int
                      [:simple-type :float] :float
                      [:pointer-type _] :int)
        lookup {:int {"+" riscv.add
                      "-" (if unary? riscv.neg riscv.sub)
                      "*" riscv.mul
                      "/" riscv.div}
                :float {"+" riscv.fadds
                         "-" (if unary? riscv.fnegs riscv.fsubs)
                         "*" riscv.fmuls
                         "/" riscv.fdivs}}]
    (assert (?. lookup simple-type operator))))

(fn riscv.cond->op [operator val-type]
  (table.unpack
   (match val-type
     [:simple-type :float]
     (match operator
       "<"  [riscv.beq riscv.flts]
       "<=" [riscv.beq riscv.fles]
       "==" [riscv.beq riscv.feqs]
       "!=" [riscv.bne riscv.feqs]
       ">=" [riscv.bne riscv.flts]
       ">"  [riscv.bne riscv.fles])

     _ (match operator
         "<"  [riscv.bge]
         "<=" [riscv.bgt]
         "==" [riscv.bne]
         "!=" [riscv.beq]
         ">=" [riscv.blt]
         ">"  [riscv.ble]))))

(fn riscv.type->store [val-type]
  (match val-type
    [:simple-type :float] riscv.fsw
    _ riscv.sw))

(fn riscv.type->load [val-type]
  (match val-type
    [:simple-type :float] riscv.flw
    _ riscv.lw))

riscv
