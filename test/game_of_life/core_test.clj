(ns game-of-life.core-test
  (:require [clojure.test :refer :all]
            [game-of-life.core :refer :all]))

(deftest test-make-grid
  (is (= 10 (count (grid 10))))
  (is (= (take 10 (repeat 10)) (map count (grid 10))))
  (is (every? (comp not :live) (flatten (grid 5)))))

(deftest test-rand-grid
  (testing "generates rand grid when given rand flag"
    (is (not (every? :live (flatten (grid 5 true)))))
    (is (not (every? (comp not :live) (flatten (grid 5 true)))))))

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

(deftest test-nextgen
  (testing "finds next value for a cell given grid"
    (let [g [[{:x 0 :y 0 :live true} {:x 1 :y 0 :live true}]
             [{:x 0 :y 1 :live true} {:x 1 :y 1 :live true}]]]
      (is (= {:x 0 :y 0 :live true} (next-gen g (first (first g))))))))

(def one-one-neighbors
  #{[0 0] [1 0] [2 0]
    [0 1]       [2 1]
    [0 2] [1 2] [2 2]})

(deftest test-neighbor-coords
  (is (= 8 (count (neighbor-coords [1 1]))))
  (is (= one-one-neighbors
         (neighbor-coords [1 1]))))

(deftest test-neighbors
  (is (= 8 (count (neighbors (grid 3) [1 1]))))
  (is (= one-one-neighbors
         (into #{}
               (map (fn [{:keys [x y]}] [x y])
                    (neighbors (grid 3) [1 1]))))))

(deftest test-retrieving-coord
  (is (= 1 (:x (grid-fetch (grid 4) [1 0]))))
  (is (= 1 (:y (grid-fetch (grid 4) [0 1])))))

(deftest test-ticking-grid
  (let [g (assoc-in (grid 3) [0 0 :live] true)
        g2 (tick g)]
    (is (not (get-in g2 [0 0 :live])))))
