;;;; CAMERA
;; This file defines a small hierarchy for a camera class
;; The camera has three distinct but related roles:
;; + Store a local coordinate system/translation: this includes an up/position vector,
;;   but the 'forward' vector will be stored in a different manner depending on a subclass
;;   this data is used to generate a view matrix
;; + Store a projection matrix
;; + Store the viewport dimensions (width, height)

;;; as always, start with *all* generic functions implemented in the file
;;; these functiosn will make more sense when seeing the camera subclasses
(in-package :glint.graphics)

(defgeneric gen-view-matrix (camera)
  (:documentation "Gets the view matrix for a particular camera"))


(defgeneric camera-rect-direction (camera)
  (:documentation "Gets the direction the camera is facing, expressed as a unit-vector"))


;; This generic function was commented out because it's not implemented, but this is a TODO item
;; (defgeneric camera-polar-direction (camera)
;;   (:documentation "Returns a 2-vector containing the polar angle and the azimuthal angle"))

;;; CAMERA Root class
;; This class is the root of the 'camera hierarchy', and should not be initialized!
;; I'm currently looking into ways to prevent this... looks like an 'abstract-class'
;; type effect can be defined when taking advantage of a MOP library
(defclass camera ()
  ((position
    :type (vector float 3)
    :accessor camera-position
    :initarg :position
    :initform (vector 0.0 0.0 0.0)
    :documentation "The position of the camera")
   (up
    :type (vector float 3)
    :accessor camera-up
    :initarg :up
    :initform (vector 0.0 1.0 0.0)
    :documentation "Gives the orientation of the camera")

   ;; projection matrix,of use when rendering to viewport 
   (projection
    :type matrix:matrix
    :accessor camera-projection
    :initarg :projection
    :documentation "The projection matrix for the camera")

   ;; viewport dimensions
   (width
    :type fixnum
    :accessor camera-width
    :initarg :width
    :accessor camera-width)
   (height
    :type fixnum
    :accessor camera-height
    :initarg :height
    :accessor camera-height))
  (:documentation "A camera is an entity used to calculate the view and
projection matrices for a particular screen/window(s)."))



;;; POLAR CAMERA
;; A camera with it's direction vector in polar coordinates, i.e. a 2-vector of
;; + Theta (Polar Angle):   The angle 'downwards' from the +ve y-axis
;; + Phi (Azimuthal Angle): The angle clockwise from +ve x-axis
(defclass polar-camera (camera)
  ((polar-direction
    :type (vector float 2)
    :accessor camera-polar-direction
    :initarg :direction
    :initform (vector (/ pi 2) 0.0)
    :documentation "The look direction in polar coordinates"))
  (:documentation "A camera with it's direction vector stored in polar coordinates"))


(defgeneric rotate-polar (camera amt)
  (:documentation "Rotate an entity with a polar coords. Requires amt to be a
2-vector containing the polar and azimuthal angles, respectively"))


(defmethod rotate-polar ((camera camera) amt)
  (setf (camera-polar-direction camera) (vec:+ (camera-polar-direction camera) amt)))


(defun rotate-polar-msg (amt) (lambda (e) (rotate-polar e amt)))


(defmethod gen-view-matrix ((camera polar-camera))
  (with-slots (polar-direction position up) camera
    (let* (;; alias theta/phi, for readability/convenience
           (theta  (elt polar-direction 0))
           (phi    (elt polar-direction 1))
           ;; dir is the rectangular unit-vector denoting the same direction
           (dir (vector (* (sin theta) (cos phi))
                        (cos theta)
                        (* (sin theta) (sin phi)))))

      ;; TODO: why scale by -1?? Is it good/bad?
      (matrix:look-at position (vec:scale -1 dir)  up))))


(defmethod camera-rect-direction ((camera polar-camera))
  (with-slots (polar-direction position up) camera
    (let* ((theta  (elt polar-direction 0))
           (phi    (elt polar-direction 1))
           (dir (vector (* (sin theta) (cos phi))
                        (cos theta)
                        (* (sin theta) (sin phi)))))
      (vec:scale -1 dir))))


(defmethod translate ((camera camera) amt) 
  (setf (camera-position camera) (vec:+ (camera-position camera) amt)))




;;; RECTANGULAR CAMERA
;; a camera with it's direction stored as a unit-vector
(defclass rectangular-camera (camera)
  ((direction
    :type (vector float 3)
    :accessor camera-direction
    :initarg :direction
    :initform (vector 0 0 -1)
    :documentation "Unit vector denoting direction camera is looking in"))
  (:documentation "A camera which stores it's look direction as a unit vector"))

(defmethod gen-view-matrix ((camera rectangular-camera))
  (with-slots (direction position up) camera
    (matrix:look-at position direction up)))

(defmethod camera-rect-direction ((camera rectangular-camera))
  (camera-direction camera))




;;; ABSOLUTE CAMERA
;; a camera storing not a direction, but a single point which is the 'target'
;; i.e. the thing the camera is looking at
(defclass absolute-camera (camera)
  ((target
    :type (vector float 3)
    :accessor camera-target
    :initarg :target
    :initform (vector 0.0 0.0 0.0)
    :documentation "The position of the point the camera is looking at"))
  (:documentation "A camera which stores it's direction as a target the 3D point is looking at"))


(defmethod gen-view-matrix ((camera absolute-camera))
  (with-slots (target position up) camera
    (matrix:look-at
     position
     (vec:normalize (vec:- target position))
     up)))
