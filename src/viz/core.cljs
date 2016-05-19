(ns viz.core)

;;----------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------

(def phone-width 240)
(def phone-height (-> phone-width (/ 9) (* 16)))

(def frame-width 360)
(def frame-x 24)
(def frame-y 99)

(def phone-x 200)
(def phone-y 200)

(def space-width 1000)
(def space-height 800)

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
  {:viewport {:scale 1
              :x 0
              :y 60}})

(def state (atom initial-state))

;;----------------------------------------------------------------------
;; Draw Page
;;----------------------------------------------------------------------

(defn draw-placeholder [ctx]
  (let [img (js/document.getElementById "placeholder")]
    (.drawImage ctx img 0 0)))

;; from: http://www.colourlovers.com/palette/141533/Not_Another_Rainbow
(def colors
  ["#6AA394"
   "#BCE48E"
   "#F8FF85"
   "#F3B55D"
   "#8B3E48"])

(def page-alpha 0.4)
(def bg-color "#f5f5f5")

(def section-height 220)
(def section-width phone-width)
(def section-pad 10)

(defn draw-page [ctx]
  (doseq [color colors]
    (set! (.. ctx -fillStyle) color)
    (.fillRect ctx 0 0 section-width section-height)
    (.translate ctx 0 section-height)
    (set! (.. ctx -fillStyle) bg-color)
    (.fillRect ctx 0 0 section-width section-pad)
    (.translate ctx 0 section-pad)))

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
    (.restore ctx)))

(defn draw []
  (draw-page-on-phone)
  (draw-page-on-space)
  (draw-phone-on-space))

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
    (set! space-ctx (.getContext canvas "2d"))
    (set! space-canvas canvas)))

(defn init []
  (init-phone)
  (init-frame)
  (init-space)
  (draw))

(init)
