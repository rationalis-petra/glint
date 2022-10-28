(in-package :glint.physics)

;; + respond to forces
(defclass physics-object (transform)
  ((mass
    :type float
    :initform 1.0)
   (velocity
    :type (vector float 3)
    :initform #(0.0 0.0 0.0))
   (acceleration
    :type (vector float 3)
    :initform #(0.0 0.0 0.0)))
(:documentation "The physics object class represents entities which can be
affected by forces"))


(defclass physical-object (transform)
  ()
(:documentation "The physical object class represents entities which can
collide, i.e. have a physical presence."))

(defgeneric colliding-p (obj1 obj2))


;; a force-field is a function which 
(defgeneric calc-force (field object))


(defclass gravity-point-field (entity)
  ((position
    :type (vector float 3)
    :initarg :position)
   (strength
    :type float
    :initform 1.0
    :initarg :strength))
  (:documentation "gravity field associated with a specific object (planet,
stellar body, etc.) "))

(defmethod calc-force ((field gravity-point-field) (object physics-object))
  ;; use F = (strength×m1×m2)/(r^2)
  (with-slots ((centre position) strength) field
    (/ (* strength ) (expt (vec:magnitude (vec:- centre (transform-position object))) 2))))
