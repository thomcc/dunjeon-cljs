(ns dunjeon-cljs.client.main
  (:refer-clojure :exclude [find])
  (:require [noir.cljs.client.watcher :as watcher]
            [clojure.browser.repl :as repl]
            [crate.core :as crate]
            [goog.array :as garray])
  (:use [jayq.core :only [$ append bind inner hide find prepend]])
  (:use-macros [crate.macros :only [defpartial]]))

;;************************************************
;; Dev stuff
;;************************************************
(set! *print-fn* (fn [& args] (dorun (map #(.log js/console %) args))))
;(watcher/init)
;(repl/connect "http://localhost:9000/repl")

;;************************************************
;; Code
;;************************************************

(declare add-msg)

(def game-width 50)
(def game-height 50)
(def canv-width (* 11 game-width))
(def canv-height (* 11 game-height))

(defn random [min range] (+ min (rand-int range)))
(defn rand-elt [set] (rand-nth (vec set)))
(defn signum [x] (if (zero? x) x (if (pos? x) 1 -1)))
(defn abssq [[x y]] (+ (* x x) (* y y)))
(defn dist [p0 p1] (abssq (map - p0 p1)))

(defrecord Tile-lookup [floor wall stairs gold booze player monster])

(def char-rep (Tile-lookup. "." "#" ">" "$" "!" "@" "m"))

(def color-rep (Tile-lookup. "white" "gray" "orange" "yellow" "magenta" "#0f0" "red"))
(def mem-color-rep (Tile-lookup. "gray" "#333" "#8c4618" "#9c9c618" "#640064" "#0f0" "#800000"))
;; (def char-rep {:floor ".", nil "#", :stairs ">", :gold "$", :booze "!",:player "@", :monster "m"})
;; (def color-rep {:floor "white", nil "gray", :stairs "orange", :gold "yellow", :booze "magenta", :monster "red",
;;                 :player "green"})
;; (def mem-color-rep {:floor "gray", :stairs "#8c4618", :booze "#640064" nil "#333", :gold "#9c9618", :monster "#800000"
;;                     :player "#408000"})

(def auto-use #{:gold, :booze})

(def distributions {:gold (constantly 5), :booze #(random 3 4), :stairs (constantly 1)})

(def directions {:north [0 -1], :south [0 1], :east [1 0], :west [-1 0]})


(defn intersects [{x0 :x, y0 :y, w0 :w, h0 :h}, {x1 :x, y1 :y, w1 :w, h1 :h}]
  (and (or (and (>= x0 x1) (<= x0 (+ x1 w1))) (and (>= x1 x0) (<= x1 (+ x0 w0))))
       (or (and (>= y0 y1) (<= y0 (+ y1 h1))) (and (>= y1 y0) (<= y1 (+ y0 h0))))))

(defn array-2d [h] (let [a (array)] (loop [i 0] (when (< i h) (aset a i (array)) (recur (inc i)))) a))

(defn shuffle [v] (doto (to-array v) garray/shuffle))

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

(defrecord Level [width height seen points tiles])

(defn levelify-map [{:keys [width, height, rooms, paths]}]
  (let [ps (merge (zipmap (mapcat pointify rooms) (repeat :floor))
                  (zipmap (apply concat paths) (repeat :floor)))
        ary (array)]
    (loop [y 0]
      (when (< y height)
        (let [row (array)]
          (loop [x 0]
            (when (< x width)
              (let [t (ps [x y])]
                (aset row x t)
                (recur (inc x)))))
          (aset ary y row)
          (recur (inc y)))))
    (Level. width height (array-2d height) ps ary)))


(defn place-randomly [{tiles :tiles :as level} n tile]
  (reduce (fn [{p :points :as l} t]
            (let [[xx yy :as pos] ((rand-elt p) 0)]
              (aset (aget tiles yy) xx t)
              (assoc-in l [:points pos] t)))
          level (repeat n tile)))

(defrecord Monster [pos health])

(defn make-monster [pos] (Monster. pos 10))

(defn add-monsters [{:keys [points] :as level} n]
  (assoc level :monsters (set (map make-monster (map #(% 0) (take n (shuffle points)))))))


(defn finalize [level]
  (add-monsters (reduce (fn [lvl [item, num-fn]] (place-randomly lvl (num-fn) item)) level distributions) (random 5 5)))

(defn gen-level [width height rooms]
  (-> (empty-map width height) (add-rooms rooms) connect-rooms levelify-map finalize))

;; vision
(defn can-see? [{tiles :tiles} [sx sy :as source] [ex ey :as end]]
  (or (= source end)
      (let [[destx desty] (map #(+ %2 (/ (signum (- %1 %2)) 2.0)) source end)
            distx (- destx sx),
            disty (- desty sy),
            length (max (Math/abs distx) (Math/abs disty))
            dx (/ distx length)
            dy (/ disty length)]
        (loop [len length, px sx, py sy]
          (or (neg? len)
              (let [mx (Math/floor (+ px 0.5)), my (Math/floor (+ py 0.5))]
                (cond (and (== mx destx) (== my desty)) true,
                      (and (not (and (== mx sx) (== my sy)))
                           (nil? (aget (aget tiles my) mx))) false
                      true (recur (dec len) (+ dx px) (+ dy py)))))))))

;; (defn can-see? [{pts :points} source end]
;;   (or (= source end)
;;       (let [dest (map #(+ %2 (/ (signum (- % %2)) 2.0)) source end)
;;             [distx disty :as dist] (map - dest source)
;;             length (max (Math/abs distx) (Math/abs disty))
;;             delta (map #(/ % length) dist)]
;;         (loop [len length, pos source]
;;           (or (neg? len)
;;               (let [moved (map (comp Math/floor (partial + 0.5)) pos)]
;;                 (cond (= moved dest) true
;;                       (and (not (= moved source)) (not (pts moved))) false
;;                       :else (recur (dec len) (map + delta pos)))))))))

(defn clear-array [a] (garray/forEach a garray/clear))

(defn update-vision
  [{{seen :seen, w :width, h :height :as lvl} :level, {sees :sees [x y] :pos :as player} :player
    :as game-state}]
  (clear-array sees)
  (loop [yy -10]
    (when (< yy 10)
      (loop [xx -10]
        (when (< xx 10)
          (let [vis (and (<= (+ (* xx xx) (* yy yy)) 100) (can-see? lvl [x y] [(+ x xx) (+ y yy)]))]
            (when-not (or (neg? (+ yy y)) (neg? (+ xx x))
                          (>= (+ y yy) h) (>= (+ x xx) w))
              (aset (aget sees (+ yy y)) (+ xx x) vis)
              (aset (aget seen (+ yy y)) (+ xx x) (or (aget (aget seen (+ yy y)) (+ xx x)) vis))))
          (recur (inc xx))))
      (recur (inc yy))))
  game-state)
;
;; (defn update-vision [{{seen :seen :as lvl} :level,{[x y] :pos :as player} :player :as game-state}]
;;   (let [visible (for [xx (range -10 10), yy (range -10 10)
;;                       :when (and (<= (abssq [xx yy]) 100)
;;                                  (can-see? lvl [x y] [(+ x xx) (+ y yy)]))]
;;                   [(+ xx x) (+ yy y)])]
;;     (-> game-state
;;         (assoc-in [:level :seen] (reduce conj seen visible))
;;         (assoc-in [:player :sees] (set visible)))))

(defrecord GameState [level player messages])

(defrecord Player [pos health sees score level dead])

(defn initialize-gamestate []
  (let [level (gen-level game-width game-height (random 5 4))]
    (-> (GameState. level (Player. ((rand-elt (:points level)) 0), 50, (array-2d 50) 0 0 false) ())
        update-vision
        (add-msg "(entering (dungeon))")
        (add-msg "Entered level 0."))))

(defn clear-tile [{{pts :points tiles :tiles} :level :as gs} [x y :as pos]]
  (aset (aget tiles y) x :floor)
  (assoc-in gs [:level :points pos] :floor))
(defmulti use-tile (fn [gs pos item] item))
(defmethod use-tile :default [gs _ _] gs)
(defmethod use-tile :floor [gs _ _] gs)
(defmethod use-tile :booze [{{h :health l :level} :player :as game-state} pos _]
  (let [healed (random 8 8), hnow (min (+ (* 5 l) 50) (+ h healed)), del (- hnow h)]
    (-> game-state
        (clear-tile pos)
        (assoc-in [:player :health] hnow)
        (add-msg (str "The booze heals you for " (if (zero? del) "no" del) " points.")))))

(defmethod use-tile :gold [game-state pos _]
  (-> game-state
      (clear-tile pos)
      (update-in [:player :score] + 25)
      (add-msg (str "You find 25 gold on the ground. Score!"))))

(defmethod use-tile :stairs [{p :player :as game-state} _ _]
  (let [next-floor (gen-level 50 50 (random 4 5))]
    (-> game-state
        (assoc :level next-floor)
        (assoc-in [:player :pos] ((rand-elt (:points next-floor)) 0))
        (update-in [:player :level] inc)
        (add-msg "You go down the stairs."))))

(defn fight-pm [{{ms :monsters} :level :as gs} {h :health :as mon}]
  (let [d (random 3 5), left (- h d), damaged (assoc mon :health left), alive? (pos? left), ms (disj ms mon)]
    (-> gs
        (assoc-in [:level :monsters] (if alive? (conj ms damaged) ms))
        (add-msg (str "The monster " (if alive? (str "takes " d " damage.") "dies.")))
        (update-in [:player :score] + (if alive? 0 (random 25 25))))))

(defn monster-at [{{mons :monsters} :level} pos]
  (loop [[{p :pos :as m} & ms] (seq mons)]
    (cond (= p pos) m, (not ms) nil, :else (recur ms))))

(defn autoheal [{{l :level h :health} :player :as gs}]
  (if (and (< h 50) (zero? (rand-int 5))) (update-in gs [:player :health] + (rand-int (+ l 3))) gs))

(defmulti tick-player (fn [gamestate [kind & args]] kind))
(defmethod tick-player :default [game-state _] game-state)
(defmethod tick-player :action [{{p :pos} :player, {pts :points} :level :as gs} _] (use-tile gs p (pts p)))

(defmethod tick-player :move [{{pos :pos h :health} :player, level :level :as gs} [_ dir]]
  (let [newpos (map + pos (directions dir)), tile ((:points level) newpos), mon (monster-at gs newpos)]
    (cond (not tile) gs
          mon (fight-pm gs mon)
          (auto-use tile) (use-tile (assoc-in gs [:player :pos] newpos) newpos tile)
          :else (assoc-in gs [:player :pos] newpos))))

(defn die [{{:keys [level score]} :player :as gs}]
  (-> gs
      (assoc-in [:player :dead] true)
      (assoc-in [:player :health] 0)
      (add-msg "red" (str "You have died on level " level " with " score " points."))
      (add-msg "red" "Press enter to try again.")))

(defn attack-player [{{l :level h :health :as player} :player :as gs}]
  (if (zero? (rand-int 4)) (add-msg gs "You dodge the monster's attack!")
      (let [dam (random (+ l 3) (+ l 7))
            next-gs (-> gs
                        (update-in [:player :health] - dam)
                        (add-msg (str "The monster attacks you for " dam " damage!")))]
        (if-not (pos? (- (:health (:player gs)) dam))
          (die (assoc-in next-gs [:player :dead] true))
          next-gs))))

(defn tick-monster [{{ms :monsters pts :points :as lvl} :level {pp :pos} :player :as gs} {mp :pos :as mon}]
  (let [vis (can-see? lvl mp pp),
        del (if vis (map (comp signum -) pp mp) (rand-nth (vals directions))),
        pos (map + mp del)]
    (cond (= pos pp) (attack-player gs)
          (pts pos) (-> gs (update-in [:level :monsters] disj mon)
                        (update-in [:level :monsters] conj (assoc mon :pos pos)))
          :else gs)))

(defn tick-monsters [{{ms :monsters, pts :points} :level {pp :pos} :player :as gs}] (reduce tick-monster gs ms))

(defn tick [{{dead? :dead} :player :as game-state} input]
  (cond (= input [:noop]) game-state
        (not dead?) (-> game-state (tick-player input) autoheal tick-monsters update-vision)
        (and dead? (= input [:action])) (initialize-gamestate)
        :else game-state))





(def $content ($ :#content))


(defpartial canv [] [:canvas#canvas {:width canv-width :height canv-height}])

;(defpartial msg-log [] [:div#msgs])

(defpartial p-msg [txt col] [:p.msg txt])

;(append $content (canv))
;(append $content (msg-log))



(def $msgs ($ :div.msgs))

(def $status ($ :#status))

(def cvs (.get ($ :#canvas) 0))

(def ctx (.getContext cvs "2d"))

(defn- set-fill [ctx col] (set! (.-fillStyle ctx) col))

(defn add-msg
  ([gs msg] (add-msg gs "white" msg))
  ([gs col message]
     (prepend $msgs ($ (p-msg message col)))
     gs))

(defpartial stat [{h :health s :score lv :level}]
  [:div
   [:p.health (str "Health: " h)]
   [:p.score (str "Score: " s)]
   [:p.level (str "Level: " lv)]])

(defn draw [{{[px py :as pos] :pos h :health s :score sees :sees, lv :level :as play} :player,
             {:keys [points width height monsters seen tiles]} :level, msgs :messages}]
  (set! (.-font ctx) "12px monospace")
  (set! (.-fillStyle ctx) "black")
  (.fillRect ctx 0 0 canv-width canv-height)
  (let [mpts (set (map :pos monsters))]
    (loop [yy 0]
      (when (< yy height)
        (loop [xx 0]
          (when (< xx width)
            (let [p [xx yy]
                  typ (name (or (cond (= p pos) :player,
                                      (mpts p) :monster,
                                      true (aget (aget tiles yy) xx))
                                :wall))
                  ch (aget char-rep typ)
                  co (cond (not (aget (aget seen yy) xx)) "black",
                           (not (aget (aget sees yy) xx)) (aget mem-color-rep typ),
                           true (aget color-rep typ))]
              (doto ctx (set-fill co) (.fillText ch (* xx 11) (* yy 11)))
              (recur (inc xx)))))
        (recur (inc yy)))))
  (inner $status (stat play)))

(def key-table
  (let [action-table
        {[:move :east] #{39}, [:move :west] #{37},[:move :north] #{38}, [:move :south] #{40}, [:action] #{13}}]
    (apply merge (for [[act keys] action-table] (zipmap (vec keys) (repeat act))))))

(def game (atom nil))

(defn init []
  (reset! game (initialize-gamestate))
  (bind ($ js/window) :keydown
        (fn [e]
          (. e (preventDefault))
          (when-let [input (key-table (.-keyCode e))]
            (swap! game tick input)
            (draw @game))))
  (draw @game))

(init)

;(.log js/console (time (reduce (fn [game _] (draw game) game) @game (range 100))))







