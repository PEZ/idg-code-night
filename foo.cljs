(enable-console-print!)

(comment
  [["("]
   ["a"]])
([])

(comment
  '([] [] [] [])
  (foo bar
       baz))

(def initial-state (create-state ""
                                 "###  "
                                 "  ###"
                                 "  #  "
                                 " ### "))

(defonce app-state-atom (atom initial-state))

(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])

([]
 [])

(map + 
     [1 2 3]
     [4 57])
     [4 57])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= 0 (distance [0 0] [0 0]))
           (is= 2 (distance [1 1] [3 1]))
           (is= 2 (distance [1 1] [3 3]))
           (is= 3 (distance [4 3] [1 1]))
           (is= 1 (distance [1 1] [1 2])))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
                
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})
  
  [[1] [2]]

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    
(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    

(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    

(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    
(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    
(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    
(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        
    
(ns game-of-life.core
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (toggle-cell [0 0])
                   (cell-alive? [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"
            (ns game-of-life.core))
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.set]))

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


(comment
  (foo
    ;;
   bar
         ;foo
   baz))

; Higher order functions
; map [f coll*]
;
; (map inc [1 3 7]) => [2 4 8]
;
; (map (fn [x] (inc x)) [1 3 7]) => [2 4 8]
;
; (map inc [1 3 7] [2 4 9]) => Wrong number of args (2) passed to inc
;
; (map + [1 3 7] [2 4 9]) => [3 7 16]
;
; (map (fn [x y] (+ x y)) [1 3 7] [2 4 9]) => [3 7 16]
;
;
; apply [f coll]
;
; (apply max [4 2 3 -1]) => 4
;
; (max 4 2 3 -1) => 4

(map inc [1 3 4])
(map (fn [x] (+ x 1)) [1 3 4])


(map + [1 2 3] [2 5 7])

(defn distance
  "Calculates the distance between two cells."
  {:test (fn []
           (is= (distance [0 0] [0 0]) 0)
           (is= (distance [1 1] [3 1]) 2)
           (is= 2 (distance [1 1] [3 3]))
           (is= 1 (distance [1 1] [1 2]))
           (is= (distance [4 3] [1 1]) 3))}
  [cell-1 cell-2]
  (->> (map - cell-1 cell-2)
       (map abs)
       (apply max)))

(defn neighbours?
  "Determines if cells are neighbours."
  {:test (fn []
           (is-not (neighbours? [0 0] [5 5]))
           (is-not (neighbours? [0 0] [0 0]))
           (is (neighbours? [0 0] [0 1]))
           (is (neighbours? [0 0] [1 1])))}
  [cell-1 cell-2]
  (= (distance cell-1 cell-2) 1))

;; To map with an index, use map-indexed [f coll]
;; then f is two variable function of index and item.


(apply + (map inc (filter even? [1 2 3 4 5 6 7 8])))

(defn create-state
  "Creates the state model for the game."
  {:test (fn []
           (is= (create-state " #  "
                              "####"
                              "  # ")
                {:living-cells #{[2 2] [1 0] [1 1] [3 1] [2 1] [0 1]}, :size [4 3]})
           (is= (create-state "#")
                {:living-cells #{[0 0]}, :size [1 1]}))}
  [& strings]
  {:living-cells (->> strings
                      (map-indexed (fn [y string]
                                     (map-indexed (fn [x character]
                                                    (when (= character \#)
                                                      [x y]))
                                                  string)))
                      (apply concat)
                      (remove nil?)
                      (into #{}))
   :size         [(count (first strings)) (count strings)]})

(defn neighbours-of
  "Returns a set of all cells that are neighbours to the given cell."
  {:test (fn []
           (is= (neighbours-of [0 0])
                #{[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]})
           (is= (neighbours-of [2 2])
                #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]}))}
  [cell]
  (->> [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
       (map (fn [direction]
              (map + cell direction)))
       (into #{})))

(defn cell-alive?
  "Determines if the given cell is alive."
  {:test (fn []
           (let [state (create-state "# ")]
             (is (cell-alive? state [0 0]))
             (is-not (cell-alive? state [1 0]))
             (is-not (cell-alive? state [-10 -20]))))}
  [state cell]
  (contains? (:living-cells state) cell))

(defn alive-neighbours-of
  {:test (fn []
           (is= (-> (create-state "##"
                                  " #")
                    (alive-neighbours-of [0 0]))
                #{[1 0] [1 1]}))}
  [state cell]
  (->> (neighbours-of cell)
       (filter (fn [c]
                 (cell-alive? state c)))
       (into #{})))
;
;; 1 Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
;; 2 Any live cell with two or three live neighbours lives on to the next generation.
;; 3 Any live cell with more than three live neighbours dies, as if by overpopulation.
;; 4 Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

;; (* 1 2 3 4 5)

(defn next-generation
  {:test (fn []
           (let [state (create-state "##"
                                     "##")]
             (is= (next-generation state)
                  state))
           (is= (-> (create-state "   "
                                  "###"
                                  "   ")
                    (next-generation))
                (create-state " # "
                              " # "
                              " # ")))}
  [state]
  (update state :living-cells
          (fn [living-cells]
            (->> living-cells
                 (map neighbours-of)
                 (apply concat)
                 (distinct)
                 (filter (fn [cell]
                           (let [number-of-alive-neighbours (count (alive-neighbours-of state cell))]
                             (cond (and (not (cell-alive? state cell))
                                        (= number-of-alive-neighbours 3))
                                   true

                                   (and (cell-alive? state cell)
                                        (or (= number-of-alive-neighbours 2)
                                            (= number-of-alive-neighbours 3)))
                                   true

                                   :else
                                   false))))
                 (into #{})))))

(defn toggle-cell
  {:test (fn []
           (is (-> (create-state "")
                   (cell-alive? [0 0])
                   (toggle-cell [0 0])))

           (is-not (-> (create-state "#")
                       (toggle-cell [0 0])
                       (cell-alive? [0 0]))))}
  [state cell]
  (update state :living-cells
          (fn [living-cells]
            (if (cell-alive? state cell)
              (clojure.set/difference living-cells #{cell})
              (conj living-cells cell)))))

(comment

  (create)
  (def s [1 2 3 4 5])
  s
  (println s)
  bork
  (reduce * s)
  (apply * s)
  (apply * (map (partial * 2) s))
  (->> s
       (map (partial * 2))
       
       (repeat 4)
       (zipmap (range 4)))

  `{1 [1 2 3] 2 [2 4 6]}
  '{:a :b :c :d}
  #{:a :b :c :d :e}

  (str ['foo] #?@(:clj ["clj"] :cljs ["cljs"]))

  #_['(1 2 3 4 5)]
  `(1 2 3 4 5)
  `{1 2 3 4}
  `{1 2 3 4}
  #{:1 :2 :3 :4 :5}
  #'s

  (def h #(str "I ❤️ " %))
  (h #?(:clj  "Clojure"
        :cljs "ClojureScript"))

  (create-state " ####   ###    #"
  
                "####   ##      #"
                "  #  ##  ###   #")

  (map (fn [_] (create-state " # ## #")) (range 2))
  (map #(str %) (range 10))
  (->> [1 2 3 4 5 6 7 8]
       (filter even?)
       (map inc)
       (apply +))

  (->> [1 2 3 4 5]
       (apply *))

  (defn foo [x]
    (str x))

  '({:a 1} {:a 1} {:a 1} {:a 1})

  (foo [1 2 3
        4 5 6
        7 8 9])

  (foobar "a"
          "b"
          "c")

  (foobar "a"
          "b")

  (create-state " ####   ###    #"
                "####   ##      #"
                "  #  ##  ###   #"))
            
        