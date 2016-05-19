(ns viz.core
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! close! <! chan tap untap mult]]))

(enable-console-print!)

;;----------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------

(def phone-width 240)
(def phone-height (-> phone-width (/ 9) (* 16)))

(def frame-width 360)
(def frame-x 24)
(def frame-y 99)

(def phone-x 280)
(def phone-y 300)

(def space-y 0)
(def space-x (+ phone-width (* 2 phone-x)))

(def space-width space-x)
(def space-height (+ phone-height (* 2 phone-y)))

;;----------------------------------------------------------------------
;; References
;;----------------------------------------------------------------------

(def phone-canvas)
(def phone-ctx)
(def space-canvas)
(def space-ctx)

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def initial-state
  {:viewport {:scale 1 :x 0 :y 60}
   :page nil
   :caption ""})

(def state (atom initial-state))

;;----------------------------------------------------------------------
;; Draw Page
;;----------------------------------------------------------------------

(defn draw-placeholder [ctx]
  (let [img (js/document.getElementById "placeholder")]
    (.drawImage ctx img 0 0)))

;; from: http://www.colourlovers.com/palette/141533/Not_Another_Rainbow
(def color-table
  ["#6AA394"
   "#BCE48E"
   "#F8FF85"
   "#F3B55D"
   "#8B3E48"])

(def block-height 220)
(def block-width phone-width)

(def block-table ;; layout for desktop page
  [{:height block-height
    :colors [0]}
   {:height (* block-height 0.7)
    :colors [1 2]}
   {:height (* block-height 0.5)
    :colors [3]}
   {:height block-height
    :colors [4]}])

(defn page-height []
  (case (:page @state)
    :mobile (* block-height (count color-table))
    :desktop (reduce + 0 (map :height block-table))
    nil))

(def page-alpha 0.4)
(def bg-color "#f5f5f5")

(def frame-color "#555")
(def frame-thickness 5)

(defn draw-mobile-page [ctx]
  (doseq [color color-table]
    (set! (.. ctx -fillStyle) color)
    (.fillRect ctx 0 0 block-width block-height)
    (.translate ctx 0 block-height)))

(defn draw-desktop-page [ctx]
  (doseq [{:keys [height colors]} block-table]
    (let [width (/ block-width (count colors))]
      (.save ctx)
      (doseq [color colors]
        (set! (.. ctx -fillStyle) (color-table color))
        (.fillRect ctx 0 0 width height)
        (.translate ctx width 0))
      (.restore ctx)
      (.translate ctx 0 height))))

(defn draw-page [ctx]
  (case (:page @state)
    :mobile (draw-mobile-page ctx)
    :desktop (draw-desktop-page ctx)
    nil))

;;----------------------------------------------------------------------
;; Draw Views
;;----------------------------------------------------------------------

(defn draw-page-on-phone []
  (let [{:keys [x y scale]} (:viewport @state)
        ctx phone-ctx]
    (.save ctx)
    (set! (.-fillStyle ctx) bg-color)
    (.fillRect ctx 0 0 phone-width phone-height)
    (.scale ctx scale scale)
    (.translate ctx (- x) (- y))
    (draw-page ctx)
    (.restore ctx)))

(defn draw-page-on-space []
  (let [{:keys [x y scale]} (:viewport @state)
        ctx space-ctx]
    (.save ctx)
    (.translate ctx phone-x phone-y)
    (.scale ctx scale scale)
    (.translate ctx (- x) (- y))
    (set! (.-globalAlpha ctx) page-alpha)
    (draw-page ctx)
    (.restore ctx)))

(defn draw-phone-on-space []
  (let [ctx space-ctx]
    (.save ctx)
    (.drawImage ctx phone-canvas phone-x phone-y)
    (set! (.-strokeStyle ctx) frame-color)
    (set! (.-lineWidth ctx) frame-thickness)
    (.strokeRect ctx phone-x phone-y phone-width phone-height)
    (let [x (/ phone-x 2)
          y (+ phone-y (/ phone-height 2))]
      (set! (.-font ctx) "300 40px Roboto")
      (set! (.-textAlign ctx) "center")
      (set! (.-textBaseline ctx) "middle")
      (set! (.-fillStyle ctx) frame-color)
      (.fillText ctx (:caption @state) x y))
    (.restore ctx)))

(defn draw []
  (.clearRect phone-ctx 0 0 phone-width phone-height)
  (.clearRect space-ctx 0 0 space-width space-height)
  (draw-page-on-phone)
  (draw-page-on-space)
  (draw-phone-on-space))

(defn draw-loop []
  (draw)
  (js/requestAnimationFrame draw-loop))

;;----------------------------------------------------------------------
;; Animation
;;----------------------------------------------------------------------

(def tick-chan
  "This channel receives dt (delta time from last frame) in milliseconds."
  (chan))

(def tick-tap
  "Allows anything to tap the tick channel (e.g. for animation)."
  (mult tick-chan))

(def prev-time
  "Timestamp of the last tick."
  nil)

(defn tick!
  "Creates heartbeat by hooking requestAnimationFrame to tick-chan."
  [curr-time]
  (let [delta-ms (if prev-time
                   (- curr-time prev-time)
                   (/ 1000 60))
        dt (/ delta-ms 1000)]
    (set! prev-time curr-time)
    (put! tick-chan dt))
  (.requestAnimationFrame js/window tick!))

(defn start-ticking! []
  (.requestAnimationFrame js/window tick!))

(def tweens
  ;; find more: https://github.com/danro/jquery-easing/blob/master/jquery.easing.js
  {:linear identity
   :swing #(- 0.5 (/ (Math/cos (* % Math/PI)) 2))}) ;; from jquery

(defn resolve-tween
  "Resolve the tween to a function if it's a name."
  [tween]
  (cond-> tween
    (keyword? tween) tweens))

(defn animate!
  "Pass given animation values to the given callback.
   Returns a channel that closes when done."
  [state-path {:keys [a b duration tween] :or {tween :swing} :as opts}]
  (let [tween (resolve-tween tween)
        c (chan)
        resolve-var #(if (= % :_) (get-in @state state-path) %)
        a (resolve-var a)
        dv (- b a)]
    (tap tick-tap c)
    (go-loop [t 0]
      (let [dt (<! c)
            t (+ t dt)
            percent (-> (/ t duration)
                        (min 1)
                        (tween))
            v (+ a (* percent dv))]
        (swap! state assoc-in state-path v)
        (when (< t duration)
          (recur t)))
      (untap tick-tap c))))

(defn multi-animate!
  "Helper for concurrent animations with `animate!`.
   Returns a channel that closes when all are done."
  [pairs]
  (let [anims (mapv #(apply animate! %) pairs)]
    (go
      (doseq [a anims]
        (<! a)))))

;;----------------------------------------------------------------------
;; Init
;;----------------------------------------------------------------------

(defn init-frame []
  (let [frame (js/document.getElementById "frame")
        x (- phone-x frame-x)
        y (- phone-y frame-y)
        scale (/ phone-width frame-width)]
    (set! (.. frame -style -left) (str x "px"))
    (set! (.. frame -style -top) (str y "px"))
    (aset (.. frame -style) "transform-origin" (str frame-x "px " frame-y "px"))
    (set! (.. frame -style -transform) (str "scale(" scale ")"))))

(defn init-phone []
  (let [canvas (js/document.getElementById "phone-canvas")]
    (set! (.. canvas -width) phone-width)
    (set! (.. canvas -height) phone-height)
    (set! (.. canvas -style -left) (str phone-x "px"))
    (set! (.. canvas -style -top) (str phone-y "px"))
    (set! phone-ctx (.getContext canvas "2d"))
    (set! phone-canvas canvas)))

(defn init-space []
  (let [canvas (js/document.getElementById "space-canvas")]
    (set! (.. canvas -width) space-width)
    (set! (.. canvas -height) space-height)
    (set! (.. canvas -style -left) (str space-x "px"))
    (set! (.. canvas -style -top) (str space-y "px"))
    (set! space-ctx (.getContext canvas "2d"))
    (set! space-canvas canvas)))

(defn load-fonts!
  [families]
  (let [c (chan)]
    (.load js/WebFont (clj->js {:google {:families families} :active #(close! c)}))
    c))

(defn start-scroll-anim! []
  (swap! state assoc
    :page :mobile
    :caption "viewport =")
  (let [margin 30
        top (- margin)
        bottom (- (+ (page-height) margin) phone-height)]
    (go-loop []
      (<! (animate! [:viewport :y] {:a top :b bottom :duration 3}))
      (<! (animate! [:viewport :y] {:a :_ :b top :duration 3}))
      (recur))))

(defn start-zoom-anim! []
  (swap! state assoc
    :page :desktop
    :caption "viewport =")
  (let [margin 30
        top (- margin)
        bottom (- (+ (page-height) margin) phone-height)
        mid (/ (+ top bottom) 2)]
    (go-loop []
      (<! (animate! [:viewport :y]     {:a top :b mid :duration 1}))
      (<! (animate! [:viewport :scale] {:a :_ :b 2 :duration 1}))
      (<! (animate! [:viewport :x]     {:a :_ :b (/ phone-width 2) :duration 1}))
      (<! (animate! [:viewport :y]     {:a :_ :b bottom :duration 1}))
      (<! (animate! [:viewport :x]     {:a :_ :b 0 :duration 1}))
      (<! (animate! [:viewport :scale] {:a :_ :b 1 :duration 1}))
      (<! (animate! [:viewport :y]     {:a :_ :b top :duration 1}))
      (recur))))

(defn init []
  (init-phone)
  (init-frame)
  (init-space)

  (go
    (<! (load-fonts! ["Roboto:100,300,400,700" "Open Sans:100,300"]))
    (draw-loop))

  (start-ticking!)
  (start-zoom-anim!))

(init)
