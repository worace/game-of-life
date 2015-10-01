(ns game-of-life.core
  (:require [clojure.test :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.string :refer [join]]
            [clojure.repl :refer [doc]]))

(defn rand-bool []
  (= 1 (rand-int 2)))

(defn vec-nest [matrix]
  (vec (map vec matrix)))

(defn grid
  ([size] (grid size false))
  ([size rand] (vec-nest (for [y (range size)]
                           (for [x (range size)]
                             {:x x :y y :live (if rand (rand-bool) false)})))))

(defn grid-fetch [g coords]
  (get-in g (reverse coords)))

(defn neighbor-coords [coords]
  (clojure.set/difference
   (into #{}
        (for [x-shift [1 0 -1]
              y-shift [1 0 -1]]
          (map + coords [x-shift y-shift])))
   #{coords}))

(defn neighbors [grid coords]
  (filter
   (comp not nil?)
   (map (partial get-in grid) (map reverse (neighbor-coords coords)))))

(defn next-state [cell neighbors]
  (let [live-count (count (filter :live neighbors))]
    (cond
      (< live-count 2) false
      (= live-count 2) (:live cell)
      (= live-count 3) true
      (> live-count 3) false)))

(defn next-gen [g cell]
  (assoc cell
         :live
         (next-state
          cell
          (neighbors g [(:x cell) (:y cell)]))))

(defn tick [g]
  (vec-nest (map (fn [row]
                   (map (partial next-gen g) row))
                 g)))

(defn print-grid [g]
   (join "\n"
        (map (fn [r]
         (apply str (map (fn [c]
                           (if (:live c) "X" "_"))
                         r)))
       g)))

(defn run-sim [ticks grid-size]
  (loop [ticks ticks
         g (grid grid-size true)]
    (when (> ticks 0)
      (do
        (println (str "i: " ticks "\n" "**********************************\n" (print-grid g)))
        (recur (dec ticks) (tick g))))))
