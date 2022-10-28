(defpackage :glint.graphics
  (:use :cl :glint.core :glint.math)
  (:export
   :draw
   ;; window.lisp
   :make-window :get-cursor-pos :key-is-pressed-p :new-shader-program :window-notify-tick
   :release-window

   ;; camera.lisp
   :camera :camera-position :camera-up :camera-projection :camera-width
   :camera-height :up :camera-up
   :polar-camera :polar-direction :camera-polar-direction :camera-rect-direction
   :rectangular-camera :direction :camera-direction
   :rotate-polar-msg
   :rotate-polar

   ;; model.lisp
   :model :make-vao :model-matrix :model-shader 
   :texture
   :set-shader
   :sync-model-matrix

   ;; mesh-loader
   :load-obj
   ))
