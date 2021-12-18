(fn errorf [format-str ...]
  (error (format-str:format ...)))

(fn assertf [cond format-str ...]
  (assert cond (format-str:format ...)))

(fn list->set [list]
  (collect [_ elem (ipairs list)]
    (values elem true)))

(fn lines [str]
  (icollect [line (str:gmatch "([^\n]+)\n?")]
    line))

(fn find [list predicate]
  (fn go [n]
    (when (<= n (length list))
      (if (predicate (. list n))
          (. list n)
          (go (+ n 1)))))
  (go 1))

(fn push [list value]
  (tset list (+ 1 (length list)) value)
  list)

(fn extend-list [dest-table src-table]
  (each [_ v (ipairs src-table)]
    (push dest-table v)))

{: errorf
 : assertf
 : list->set
 : lines
 : find
 : push
 : extend-list}
