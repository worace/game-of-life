(ns game-of-life.core
  (:require [clojure.test :refer :all]
            [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.repl :refer [doc]]))

(defn grid [size]
  (vec (for [y (range size)]
         (vec (for [x (range size)]
                {:x x :y y :live false})))))

(defn grid-fetch [g coords]
  (get-in g (reverse coords)))

(defn neighbors [grid coords]
  (clojure.set/difference
   (into #{}
        (for [x-shift [1 0 -1]
              y-shift [1 0 -1]]
          (map + coords [x-shift y-shift])))
   #{coords}))

(defn next-state [cell neighbors]
  (let [live-count (count (filter :live neighbors))]
    (cond
      (< live-count 2) false
      (= live-count 2) (:live cell)
      (= live-count 3) true
      (> live-count 3) false)))

(defn next-gen [g cell]
  (next-state
   cell
   (neighbors g [(:x cell) (:y cell)])))

(defn tick [g]
  (vec (map (fn [row]
              (vec (map (partial next-gen g) row)))
            g)))

(deftest test-ticking-grid
  (let [g (assoc-in (grid 3) [0 0 :live] true)
        g2 (tick g)]
    (is (not (get-in g2 [0 0 :live])))))

(deftest test-next-state
  (is (not (next-state {:x 0 :y 0 :live true} #{})))
  (is (next-state {:x 0 :y 0 :live true}
                  #{{:id 1 :live true} {:id 2 :live true}}))
  (is (next-state {:x 0 :y 0 :live true}
                  #{{:id 1 :live true} {:id 2 :live true} {:id 3 :live true}}))
  (is (not (next-state {:x 0 :y 0 :live true}
                       #{{:id 1 :live true}
                         {:id 2 :live true}
                         {:id 3 :live true}
                         {:id 4 :live true}})))
  (is (not (next-state {:x 0 :y 0 :live false}
                       #{{:id 1 :live true} {:id 2 :live true}}))))

(deftest test-make-grid
  (is (= 10 (count (grid 10))))
  (is (= (take 10 (repeat 10)) (map count (grid 10)))))

(deftest test-neighbors
  (is (= 8 (count (neighbors (grid 4) [1 1]))))
  (is (= #{[0 0] [1 0] [2 0]
           [0 1]       [2 1]
           [0 2] [1 2] [2 2]})))

(deftest test-retrieving-coord
  (is (= 1 (:x (grid-fetch (grid 4) [1 0]))))
  (is (= 1 (:y (grid-fetch (grid 4) [0 1])))))

(run-tests)
