(local {: errorf} (require :compiler.util))
(local {: token-location} (require :compiler.lexing))

(local node-defs
       {:program      [:decls :functions]
        :var-decl     [:base-type :id]
        :str-decl     [:id :init]
        :func-decl    [:base-type :id :params]
        :param        [:base-type :id]
        :function     [:base-type :id :params :decls :statements]
        :func-call    [:id :args]
        :read-stmt    [:id]
        :print-stmt   [:expr]
        :return-stmt  [:?expr]
        :assign-stmt  [:id :expr]
        :while-stmt   [:cond :statements]
        :if-stmt      [:cond :statements :?else-statements]
        :cond         [:op :l-expr :r-expr]
        :binary-op    [:op :l-expr :r-expr]
        :unary-op     [:op :expr]
        :identifier   [:symbol]
        :deref        [:expr]
        :addr-of      [:expr]
        :cast-expr    [:type :expr]

        ;; intrinsics
        :malloc       [:expr]
        :free         [:expr]

        ;; literals
        :integer-lit  [:integer]
        :float-lit    [:float]

        ;; types
        :simple-type  [:type]
        :pointer-type [:to]})

(fn node-type? [str]
  (not= nil (. node-defs str)))

(fn make-node [node-name subnodes token args]
  (each [i subnode-name (ipairs subnodes)]
    (when (not (or (subnode-name:match "^%?")
                   (. args i)))
      (errorf "Expected required argument '%s' for ast node '%s'"
              subnode-name node-name)))

  (table.insert args 1 node-name)
  (setmetatable args {:__index {:source (token-location token)}})
  args)

(local ast (collect [node-name subnodes (pairs node-defs)]
             (values node-name
                     (fn [token ...]
                       (make-node node-name subnodes token [...])))))

ast
