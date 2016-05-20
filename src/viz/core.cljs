(ns viz.core
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cljs.core.async :refer [put! close! <! chan tap untap mult timeout]]
    [clojure.string :as string]))

(enable-console-print!)

;;----------------------------------------------------------------------
;; Canvas References
;;----------------------------------------------------------------------

;; Phone canvas and drawing context
;; (the canvas inside the iphone frame)
(def phone-canvas)
(def phone-ctx)

;; Space canvas and drawing context
;; (the grey canvas displaying the whole page)
(def space-canvas)
(def space-ctx)

;;----------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------

;; Size of the phone canvas
(def phone-width 240)
(def phone-height (-> phone-width (/ 9) (* 16)))

;; Size of the screen inside the phone frame image
(def frame-width 360)

;; Offset into the phone frame image where screen starts
(def frame-x 24)
(def frame-y 99)

;; Offset of the phone canvas on our page
(def phone-x 280)
(def phone-y 300)

;; Offset of the space canvas on our page
(def space-y 0)
(def space-x (+ phone-width (* 2 phone-x)))

;; Size of the space canvas
(def space-width space-x)
(def space-height (+ phone-height (* 2 phone-y)))

;; The opacity of the page when it is outside the viewport
(def space-alpha 0.4)

;; Background color of the space canvas
(def bg-color "#f5f5f5")

;; We frame the visible viewport on the space canvas
(def border-color "#555")
(def freeze-border-color "#FFF")
(def border-thickness 5)

;;----------------------------------------------------------------------
;; State
;;----------------------------------------------------------------------

(def initial-state
  {;; Viewport (scroll and zoom)
   :viewport {:scale 1 :x 0 :y 60}

   ;; Previous viewport (saved when freezing, restored when thawing)
   :prev-viewport nil

   ;; Key of the color palette in use
   :color-key nil

   ;; Key of the page to draw
   :page-key nil

   ;; Caption to display next to the viewport
   :caption ""

   ;; Opacity of the frozen viewport border
   :freeze-border-alpha 0

   ;; When isolating an element, we lower the opacity
   ;; of all other elements (i.e. the color blocks).
   :isolate {:index nil    ;; element to isolate
             :opacity 0}}) ;; opacity of other elements

(def state (atom initial-state))

;;----------------------------------------------------------------------
;; Draw Page
;;----------------------------------------------------------------------

(defn draw-placeholder [ctx]
  (let [img (js/document.getElementById "placeholder")]
    (.drawImage ctx img 0 0)))

(def color-tables
  {:not-another-rainbow
    ;; from: http://www.colourlovers.com/palette/141533/Not_Another_Rainbow
    ["#6AA394"
     "#BCE48E"
     "#F8FF85"
     "#F3B55D"
     "#8B3E48"]

   :ablaze
    ;; from: http://www.colourlovers.com/palette/4245759/Ablaze
    ["#DCC5B3"
     "#FFD4A0"
     "#FCA474"
     "#E9835B"
     "#C26E52"]

   :sky-tonight
    ;; from: http://www.colourlovers.com/palette/4245751/the_sky_tonight
    ["#F9D8A7"
     "#DFCDBB"
     "#C1BEBB"
     "#A0A3AB"
     "#5F6C80"]

   :pond-queens
    ;; from: http://www.colourlovers.com/palette/4245664/Pond_Queens
    ["#D5EB7C"
     "#A3DD93"
     "#93CDB9"
     "#8FB0BA"
     "#8492B4"]

   :equine
    ;; from: http://www.colourlovers.com/palette/4245393/Equine
    ["#4D7096"
     "#4B90BA"
     "#60B0FB"
     "#9A6E7B"
     "#6E3042"]})

(defn color-table []
  (color-tables (:color-key @state)))

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
  (case (:page-key @state)
    :mobile (* block-height (count (color-table)))
    :desktop (reduce + 0 (map :height block-table))
    nil))

(defn draw-mobile-page [ctx]
  (.save ctx)
  (doseq [color (color-table)]
    (set! (.. ctx -fillStyle) color)
    (.fillRect ctx 0 0 block-width block-height)
    (.translate ctx 0 block-height))
  (.restore ctx))

(defn draw-desktop-page [ctx]
  (.save ctx)
  (doseq [{:keys [height colors]} block-table]
    (let [width (/ block-width (count colors))]
      (.save ctx)
      (doseq [color colors]
        (set! (.. ctx -fillStyle) (get (color-table) color))
        (.fillRect ctx 0 0 width height)
        (.translate ctx width 0))
      (.restore ctx)
      (.translate ctx 0 height)))
  (.restore ctx))

(defn draw-page [ctx]
  (case (:page-key @state)
    :mobile (draw-mobile-page ctx)
    :desktop (draw-desktop-page ctx)
    (draw-placeholder ctx)))

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

(defn transform-space-to-page [ctx]
  (let [{:keys [x y scale]} (:viewport @state)]
    (.translate ctx phone-x phone-y)
    (.scale ctx scale scale)
    (.translate ctx (- x) (- y))))

(defn draw-page-on-space []
  (let [ctx space-ctx]
    (.save ctx)
    (transform-space-to-page ctx)
    (set! (.-globalAlpha ctx) space-alpha)
    (draw-page ctx)
    (.restore ctx)))

(defn draw-prev-viewport [ctx]
  (let [{:keys [x y scale]} (:viewport @state)]
    (.save ctx)
    (transform-space-to-page ctx)
    (set! (.-globalAlpha ctx) (/ space-alpha 2))
    (when-let [{:keys [x y scale]} (:prev-viewport @state)]
      (set! (.-lineWidth ctx) border-thickness)
      (set! (.-strokeStyle ctx) border-color)
      (.strokeRect ctx x y (/ phone-width scale) (/ phone-height scale)))
    (.restore ctx)))

(defn draw-phone-on-space []
  (let [ctx space-ctx]
    (.save ctx)

    ;; Draw the view rendered to the phone.
    ;; (will act to highlight the viewport over the translucent surrounding page)
    (.drawImage ctx phone-canvas phone-x phone-y)

    ;; Draw a light border to show the previous viewport to be thawed.
    (draw-prev-viewport ctx)

    ;; Draw the current viewport frame
    (set! (.-lineWidth ctx) border-thickness)
    (set! (.-strokeStyle ctx) border-color)
    (.strokeRect ctx phone-x phone-y phone-width phone-height)

    ;; Highlight the viewport frame to communicate when its frozen.
    (set! (.-lineWidth ctx) (+ 2 border-thickness))
    (set! (.-strokeStyle ctx) freeze-border-color)
    (.save ctx)
    (set! (.-globalAlpha ctx) (:freeze-border-alpha @state))
    (.strokeRect ctx phone-x phone-y phone-width phone-height)
    (.restore ctx)

    ;; Draw viewport caption
    (set! (.-font ctx) "300 40px Roboto")
    (set! (.-textAlign ctx) "center")
    (set! (.-textBaseline ctx) "middle")
    (set! (.-fillStyle ctx) border-color)
    (let [x (/ phone-x 2)
          y (+ phone-y (/ phone-height 2))
          lines (:caption @state)
          lines (if (sequential? lines) lines [lines])]
      (doseq [line lines]
        (.fillText ctx line x y)
        (.translate ctx 0 48)))

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
;; Animation Helpers
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
;; Animations
;;----------------------------------------------------------------------

(defn start-scroll-anim! []
  (swap! state assoc
    :page-key :mobile
    :color-key :pond-queens
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
    :page-key :desktop
    :color-key :ablaze
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

(defn freeze-frame! [scale]
  (swap! state assoc :prev-viewport (:viewport @state))
  (go
    (<! (animate! [:viewport :scale] {:a :_ :b scale :duration 0.1}))
    (let [dt 100]
      (dotimes [i 6]
        (swap! state assoc :freeze-border-alpha 0)
        (<! (timeout dt))
        (swap! state assoc :freeze-border-alpha 1)
        (<! (timeout dt))))
    nil))

(defn thaw-frame! []
  (go
    (let [{:keys [x y scale]} (:prev-viewport @state)
          duration 0.1]
      (<! (multi-animate!
            [[[:viewport :x]     {:a :_ :b x :duration 0.1}]
             [[:viewport :y]     {:a :_ :b y :duration 0.1}]
             [[:viewport :scale] {:a :_ :b scale :duration 0.1}]]))
      (swap! state assoc :prev-viewport nil))
    (<! (animate! [:freeze-border-alpha] {:a 1 :b 0 :duration 1}))))

(defn start-freeze-anim! []
  (swap! state assoc
    :page-key :desktop
    :color-key :sky-tonight)
  (let [margin 30
        top (- margin)
        bottom (- (+ (page-height) margin) phone-height)
        mid (/ (+ top bottom) 2)]
    (go-loop []
      (swap! state assoc :caption "")
      (<! (animate! [:viewport :y]     {:a top :b mid :duration 1}))
      (<! (animate! [:viewport :scale] {:a :_ :b 2 :duration 2}))

      (swap! state assoc :caption ["1. Freeze" "   zoom"])
      (<! (freeze-frame! 1))
      (<! (timeout 1))

      (<! (animate! [:viewport :y]     {:a :_ :b bottom :duration 2}))

      (swap! state assoc :caption ["2. Thaw to" "   restore"])
      (<! (thaw-frame!))

      (<! (animate! [:viewport :y]     {:a :_ :b bottom :duration 1}))
      (<! (animate! [:viewport :scale] {:a :_ :b 1 :duration 1}))
      (<! (animate! [:viewport :y]     {:a :_ :b top :duration 1}))
      (recur))))

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

(defn init []
  (init-phone)
  (init-frame)
  (init-space)

  (go
    (<! (load-fonts! ["Roboto:100,300,400,700" "Open Sans:100,300"]))
    (draw-loop))

  (start-ticking!)
  ;(start-scroll-anim!))
  ;(start-zoom-anim!))
  (start-freeze-anim!))

(init)
