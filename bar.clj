(ns bar
  (:require [clojure.data :refer [diff]]))


(comment
  "([\\])"
  "([\\]\\])"
  "([\\[\\]\\])")

(comment
  (defn foo []
    (let [foo {:aaaa     1 ;; apa
               :bb       2
               :ccc      3
               :dddddd   4
               :eeeeeeee 7
               :ff       8}]))
  (defn foo []
    (let [a 1]
      (bar)))


  (-> foo)

  (print

   (foo bar
        foo
        baz))

  (keep-indexed (fn [i [line-o line-n]]
                  (when (not= line-o line-n)
                    [i [line-o line-n]]))
                [["foo" "foo"]
                 ["bar" "bar"]
                 ["foo" "bar"]])


  (map vector
       ["foo" "foo" "foo"]
       ["foo" "bar" "foo"])

  (->> (map vector
            ["foo" "foo" "bar" "bar"]
            ["foo" "bar" "baz" "bar"])
       (keep-indexed (fn [i [o n]]
                       (when (not= o n)
                         [i [o n]])))
       (map (fn [[row [o n]]]
              [:replace [[row 0] [row (count o)] n]])))

  (map identity (repeat 25 0))

  (diff ["foo" "foo" "bar" "bar"]
        ["foo" "bar" "baz" "bar"]))



