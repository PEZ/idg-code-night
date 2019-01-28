("(foo)")

(defn abs                                                   ; name
  "The absolute value of a number."                         ; doc-string?
   {:test (fn []
            (is= (abs 5) 5)
            (is= (abs -2) 2)
            (is= (abs 0) 0))}                                ; attr-map?
  [x]                                                       ; [params*]
  {:pre [(number? x)]}                                      ;  prepost-map?
  (if (pos? x)
    x
    (- x)))
