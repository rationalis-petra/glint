;;;; INPUT SYSTEM
;; This provides a basic input/output system facility in glint. The basic idea
;; is to update an input resource which can then be queried by clients

(in-package :glint.input)

(defvar *key-sensitivity* 5.0)
(defvar *mouse-sensitivity* 0.1)


(defun input-system (view)
  ;; we use key-presses of wasd/shift/space to adjust the position of the camera
  ;; and the up/sideways movement of the camera to change the polar and azimuthal
  ;; angles, respectively
  ;; the wasd keys will move the camera relative to it's forward/right vector
  (let ((camera (get-resource 'camera view))
        (time (world-delta-time (view-engine view))))
    (with-slots (position polar-direction up) camera
      (let* ((theta (elt (camera-polar-direction camera) 0))
             (phi   (elt (camera-polar-direction camera) 1))
             ;; calculate a 'forward' and 'right' vectors based on the camera's direction
             (forward (vector (* (sin theta) (cos phi))
                              (cos theta)
                              (* (sin theta) (sin phi))))
             (right (vec:cross forward (camera-up camera))))

        ;; forward/backward 
        (when (key-is-pressed-p :w)
          (setf position
                (vec:+ position (vec:scale (* time *key-sensitivity*) forward))))
        (when (key-is-pressed-p :s)
          (setf position
                (vec:+ position (vec:scale (* -1.0 time *key-sensitivity*) forward))))

        ;; left/right (x-coord)
        (when (key-is-pressed-p :a)
          (setf position
                (vec:+ position (vec:scale (* -1.0 time *key-sensitivity*) right))))
        (when (key-is-pressed-p :d)
          (setf position
                (vec:+ position (vec:scale (* time *key-sensitivity*) right))))

        ;; up/down (y-coord)
        (when (key-is-pressed-p :space)
          (setf position
                (vec:+ position (vec:scale (* time *key-sensitivity*) up))))
        (when (key-is-pressed-p :left-shift)
          (setf position
                (vec:+ position
                      (vec:scale (* -1.0 time *key-sensitivity*) up)))))

      (let ((delta-pos (world-cursor-delta-pos (view-engine view))))
        ;; index 0 = x, affects phi = index 1
        (incf (elt polar-direction 0) (* (elt delta-pos 1) time *mouse-sensitivity*))
        ;; index 1 = y, affects theta = index 0
        (incf (elt polar-direction 1) (* (elt delta-pos 0) time *mouse-sensitivity*))))))
