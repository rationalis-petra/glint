;;;; VECTOR
;; File which provices functions to operate on vectors, stored as
;; one-dimensional lisp arrays.
;; Because we sometimes use vectors of integers, we enforce the
;; looser restriction that they are numbers, not just floats
;; because we define '+' and '-', you'll notice we prefix number
;; operations with 'cl:', because they are in the common lisp
;; package



(defpackage :vec
  (:use :cl)
  (:shadow common-lisp::- common-lisp::+)
  (:export
   :+ :- :dot :cross :scale :normalize :magnitude
   :vec4 :vec3))

(in-package :vec)

(defun magnitude (vec)
  "Returns the l2 norm of a vector"
  (declare (type (vector number) vec))
  (loop for x across vec
        summing (* x x) into total
        finally (return (sqrt total))))

(defun scale (s vec)
  "Scale a vector by some number"
  (declare (type (vector number) vec) (type number s))
  (map 'vector #'*
       (make-array (length vec) :initial-element s)
       vec))

(defun normalize (vec)
  "Scale a vector so it's l2 norm is 1"
  (declare (type (vector number) vec))
  (scale (/ 1 (magnitude vec)) vec))

(defun + (left right)
  "Vector addition: requires equal length vectors"
  (declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (map 'vector #'cl:+ left right))

(defun - (left right)
  "Vector subtraction: requires equal length vectors"
  (declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (map 'vector #'cl:- left right))

(defun dot (left right)
  "Dot product: requires eqial length vectors"
  (declare (type (vector number) left right))
  (assert (= (length left) (length right)))

  (reduce #'cl:+ (map 'vector #'* left right)))

(defun cross (left right)
  "Cross product for vectors of dimension 3"
  (declare (type (vector number 3) left right))
  (vector
   (cl:- (* (aref left 1) (aref right 2))
      (* (aref left 2) (aref right 1)))
   (cl:- (* (aref left 2) (aref right 0))
      (* (aref left 0) (aref right 2)))
   (cl:- (* (aref left 0) (aref right 1))
      (* (aref left 1) (aref right 0)))))



;;; VECTOR CONSTRUCTORS
;; Currently quite simple functions, primarily for converting between different
;; length vectors, e.g. vec3/vec4. We declaim inline because they are very simple
(declaim (inline vec4))
(defun vec4 (vec num)
  (concatenate 'vector vec (vector num)))

(declaim (inline vec4))
(defun vec3 (vec)
  (subseq vec 0 3))
