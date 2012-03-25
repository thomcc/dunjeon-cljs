(ns dunjeon-cljs.client.main
  (:require [noir.cljs.client.watcher :as watcher]
            [clojure.browser.repl :as repl]
            [crate.core :as crate]
            [goog.array :as array])
  (:use [jayq.core :only [$ append]])
  (:use-macros [crate.macros :only [defpartial]]))

;;************************************************
;; Dev stuff
;;************************************************

(watcher/init)
;(repl/connect "http://localhost:9000/repl")

;;************************************************
;; Code
;;************************************************

(def game-width 50)
(def game-height 50)
(def canv-width (* 11 (+ 2 game-width)))
(def canv-height (* 11 (+ 2 game-height)))

(defn random [min range] (+ min (rand-int range)))
(defn rand-elt [set] (rand-nth (vec set)))
(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))
(defn abssq [[x y]] (+ (* x x) (* y y)))
(defn dist [p0 p1] (abssq (map - p0 p1))) ; really dist^2



(def char-rep {:floor ".", nil "#", :stairs ">", :gold "$", :booze "!",:player "@", :monster "m"})

(def color-rep {:floor "white", nil "gray", :stairs "orange", :gold "yellow", :booze "magenta", :monster "red",
                :player "green"})

(def auto-use #{:gold, :booze})

(def distributions {:gold (constantly 5), :booze #(random 3 4), :stairs (constantly 1)})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})


(defn intersects [{x0 :x, y0 :y, w0 :w, h0 :h}, {x1 :x, y1 :y, w1 :w, h1 :h}]
  (and (or (and (>= x0 x1) (<= x0 (+ x1 w1))) (and (>= x1 x0) (<= x1 (+ x0 w0))))
       (or (and (>= y0 y1) (<= y0 (+ y1 h1))) (and (>= y1 y0) (<= y1 (+ y0 h0))))))


(defn shuffle [v] (doto (to-array v) array/shuffle))

(defn empty-map [w h] {:width w, :height h, :rooms #{}})

(defn room [{:keys [width height]}]
  {:x (random 2 (- width 14)) :y (random 2 (- height 14)) :w (random 3 10) :h (random 3 10)})

(defn add-rooms
  ([level n] (nth (iterate add-rooms level) n))
  ([{rooms :rooms :as level}]
     (let [available? (fn [r] (not-any? #(intersects r %) rooms))]
       (loop [r (room level), i 0]
         (cond (available? r) (assoc level :rooms (conj rooms r))
               (> i 1000) level
               :else (recur (room level) (inc i)))))))

(defn connect [{:keys [width height]}, {fx :x, fy :y, fw :w, fh :h}, {tx :x, ty :y, tw :w, th :h}]
  (let [x0 (random fx fw), y0 (random fy fh)
        x1 (random tx tw), y1 (random ty th)
        dx (signum (- x1 x0)), dy (signum (- y1 y0))]
    (loop [x x0, y y0, points #{}, horizontal? (not= (rand-int 2))]
      (if (and (= x x1) (= y y1)) points
          (let [[x y] (if (or (= y y1) (and horizontal? (not= x x1))) [(+ x dx) y] [x (+ y dy)])]
            (if-not (and (> x 0) (> y 0) (< x width) (< y height)) points
                    (recur x y (conj points [x y]) (not= 0 (rand-int 10)))))))))

(defn connect-rooms [{:keys [width, height, rooms] :as level}]
  (loop [from (rand-elt rooms), conn #{from}, unconn (disj rooms from), paths #{}]
    (if (empty? unconn) (assoc level :paths paths)
      (let [to (rand-elt unconn), unconn (disj unconn to), conn (conj conn to)]
        (recur (rand-elt conn), conn, unconn, (conj paths (connect level from to)))))))

(defn pointify [{x :x, y :y, width :w, height :h}]
  (mapcat (fn [[row y]] (map #(vector % y) row))
          (partition 2 (interleave (repeat height (range x (+ x width)))
                                   (range y (+ y height))))))

(defn levelify-map [{:keys [width, height, rooms, paths]}]
  {:width width, :height height, :seen #{},
   :points (merge (zipmap (mapcat pointify rooms) (repeat :floor))
                  (zipmap (apply concat paths) (repeat :floor)))})

(defn place-randomly [level n tile]
  (reduce (fn [{p :points :as l} t] (assoc-in l [:points ((rand-elt p) 0)] t))
          level (repeat n tile)))

(defn make-monster [pos] {:pos pos, :health 10})

(defn add-monsters [{:keys [points] :as level} n]
  (assoc level :monsters (set (map make-monster (map #(% 0) (take n (shuffle points)))))))


(defn finalize [level]
  (add-monsters (reduce (fn [lvl [item, num-fn]] (place-randomly lvl (num-fn) item)) level distributions)
                (random 5 5)))


(defn gen-level [width height rooms]
  (-> (empty-map width height) (add-rooms rooms) connect-rooms levelify-map finalize))




(def $content ($ :#content))

(defpartial canv [] [:canvas#canvas {:width canv-width :height canv-height}])
(defpartial btn [txt] [:button#button txt])
(append $content (canv))

(def cvs (.get ($ :#canvas) 0))

(def ctx (.getContext cvs "2d"))

(def l (gen-level 50 50 (random 4 5)))

(.log js/console l)

(set! (.-font ctx) "12px monospace")

(defn- set-fill [ctx col] (set! (.-fillStyle ctx) col))

(defn draw-level [{:keys [points width height monsters] :as lvl}]
  (let [mpts (set (map :pos monsters))]
    (.log js/console monsters)
    (doseq [xx (range width), yy (range height)]
      (let [p [xx yy], type (if (mpts p) :monster (points p)), ch (char-rep type), co (color-rep type)]
        (doto ctx (set-fill co) (.fillText ch (* xx 11) (* yy 11)))))))

(draw-level l)





