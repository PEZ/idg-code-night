(ns playground)


(defn -enclosing? [{:keys [queue first? opening closing mapping]} c]
  (println closing opening mapping)
  (if (and (not first?) (empty? queue))
    (reduced {:queue (conj queue c)})
    {:first? false
     :queue (cond
              (opening c) (conj queue (mapping c))
              (closing c) (if (= c (peek queue))
                            (pop queue)
                            (reduced (conj queue c)))
              :else queue)}))

(defn enclosing? [text]
  (let [opening (set "{[(")
        closing (set "}])")
        mapping (apply assoc {} (interleave opening closing))]
    (-> (reduce -enclosing? {:queue []
                             :first? true
                             :opening opening
                             :closing closing
                             :mapping mapping}
                text)
        (:queue)
        (empty?))))

(comment
  (enclosing? "[][]")
  (enclosing? "([][])"))