(fn sym-entry->str [{: name : sym-type : addr : value}]
  (if value
      (: "; name %s type %s location 0x%08x value \"%s\""
         :format name (sym-type:upper) addr value)
      (: "; name %s type %s location 0x%08x"
         :format name (sym-type:upper) addr)))

(fn sym-tbl->string [{:global-scope scope}]
  (let [lines (icollect [_ entry (ipairs scope.syms)
                         :into [(.. "; Symbol table " (scope.name:upper))]]
                (sym-entry->str entry))]
    (table.concat lines "\n")))

(fn mk-counter [init increment]
  {:value (or init 1)
   :next (fn [self]
           (let [prev self.value]
             (set self.value (+ prev (or increment 1)))
             prev))})

(fn mk-scope [parent-scope name]
  {:syms (setmetatable {} {:__index parent-scope.syms})
   :locals-counter (mk-counter -4 -4)
   :args-counter (mk-counter 12 4)
   :subscopes []
   : name})

(fn mk-symbol-table []
  (local global-scope {:name "global"
                       :locals-counter (mk-counter 0x2000_0000 4)
                       :subscopes []
                       :syms {}})
  (local scope-stack [global-scope])

  (local string-addr-counter (mk-counter 0x1000_0000 4))

  (fn current-scope []
    (. scope-stack (length scope-stack)))

  (fn add-scope [name]
    (let [scope (mk-scope (current-scope) name)]
      (table.insert (. (current-scope) :subscopes) scope)
      (table.insert scope-stack scope)))

  (fn pop-scope []
    (let [top-scope (current-scope)]
      (table.remove scope-stack)
      top-scope))

  (fn lookup-symbol [symbol-name]
    (. (current-scope) :syms symbol-name))

  (fn add-variable [name sym-type init source]
    (if (rawget (current-scope) name)
        (values nil "Multiple declaration")
        (let [scope (current-scope)
              address (match sym-type
                        [:simple-type :string] (string-addr-counter:next)
                        _ (scope.locals-counter:next))]
          (tset scope.syms name {: sym-type
                                 : init
                                 : address
                                 : source
                                 :local? (not= scope global-scope)}))))

  (fn add-argument [name sym-type source]
    (let [scope (current-scope)]
      (assert (not= global-scope scope) "Cannot add argument symbol to global scope")
      (tset scope.syms name {: sym-type
                             : source
                             :address (scope.args-counter:next)
                             :local? true})))

  (fn add-function [name return-type arg-types defined? source]
    (if (rawget (current-scope) name)
        (values nil "Multiple declaration")
        (let [scope (current-scope)]
          (assert (= global-scope scope) "Cannot add function symbol to non-global scope")
          (tset scope.syms name {: return-type
                                 : arg-types
                                 : source
                                 :defined? (= :defined defined?)
                                 :type "function"}))))

  {:get-symbol-table #global-scope
   : current-scope
   : add-scope
   : pop-scope
   : add-variable
   : add-function
   : add-argument
   : lookup-symbol})

{: mk-symbol-table}
