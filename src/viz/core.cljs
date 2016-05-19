(ns viz.core)

;;----------------------------------------------------------------------
;; Constants
;;----------------------------------------------------------------------

(def phone-width 240)
(def phone-height (-> phone-width (/ 9) (* 16)))

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
              :x 200
              :y 60}})

(def state (atom initial-state))

;;----------------------------------------------------------------------
;; Draw
;;----------------------------------------------------------------------

(defn draw-page [ctx]
  (let [img (js/document.getElementById "placeholder")]
    (.drawImage ctx img 0 0)))

(defn draw-page-on-phone []
  (let [{:keys [x y scale]} (:viewport @state)
        ctx phone-ctx]
    (.save ctx)
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
    (set! (.-globalAlpha ctx) 0.2)
    (draw-page ctx)
    (.restore ctx)))

(defn draw-phone-on-space []
  (.drawImage space-ctx phone-canvas phone-x phone-y))

(defn draw []
  (draw-page-on-phone)
  (draw-page-on-space)
  (draw-phone-on-space))

;;----------------------------------------------------------------------
;; Init
;;----------------------------------------------------------------------

(defn init-phone []
  (let [canvas (js/document.getElementById "phone-canvas")]
    (set! (.. canvas -width) phone-width)
    (set! (.. canvas -height) phone-height)
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
  (init-space)
  (draw))

(init)
