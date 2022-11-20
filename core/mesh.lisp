(in-package :glint.core)



(defclass mesh ()
  ((vertices
    :type (vector vec:vec3)
    :accessor mesh-vertices
    :initarg :vertices
    :initform #())
   (faces
    :type (vector (vector fixnum 3))
    :accessor mesh-faces
    :initarg :faces
    :initform #()))
  (:documentation "A classical face-vertex mesh representation"))
