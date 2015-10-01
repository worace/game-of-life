(ns game-of-life.sketch
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [game-of-life.core :as gol]))

(def cell-size 5)

(defn setup []
  (q/background 200)
  (q/no-stroke)
  (gol/grid 75 true))

(defn update [grid]
  (gol/tick grid))

(defn cell-rect [{:keys [x y live]}]
  (if live
    (q/fill 0)
    (q/fill 250))
  (q/rect (* cell-size x) (* cell-size y) cell-size cell-size))

(defn draw [grid]
  (q/background 200)
  (doseq [cell (flatten grid)]
    (cell-rect cell)))

(q/defsketch game-of-life
  :middleware [m/fun-mode]
  :title "Game of Life"
  :update update
  :setup setup
  :draw draw
  :size [600 600])
