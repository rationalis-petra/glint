;;;; MATRIX
;; This file is self-explanatory: contains functions which manipulate matrices
;; as well as create various types of matrix relevant to 3D graphics: rotation,
;; scale, ... Generally, will create 4x4 matrices, but can operate on matrices
;; of any size
;; As elsewhere, we use floats: both for efficiency purposes and smooth OpenGL
;; interop
;; because we define '+' and '-', you'll notice we prefix number
;; operations with 'cl:', because they are in the common lisp
;; package


(defpackage :matrix
  (:use :cl :vec)
  (:shadow common-lisp::identity common-lisp::+ common-lisp::* common-lisp::apply
           vec::- vec::+ vec::scale)
  (:export
   :matrix
   :identity :scale :rotate :translate
   :look-at :perspective :detailed-perspective
   :+ :* :apply :inverse :det))

(in-package :matrix)

;;; MATRIX STRUCT
;; The matrix is defined as a struct because structs will allow for more optimization later on
;; Data is stored in a row-major order
(declaim (inline make-matrix))
(defstruct matrix
  (rows 0 :type fixnum)
  (cols 0 :type fixnum)
  (data #() :type (vector float)))

;;; This function is utility and *not* exported by the package
(declaim (inline mref))
(defun mref (matrix i j)
  "Gets element at row i, col j, does no bounds-checking!"
  (aref (matrix-data matrix) (cl:+ j (cl:* i (matrix-cols matrix)))))
(defun mref-upd (matrix i j new)
  (setf (aref (matrix-data matrix) (cl:+ j (cl:* i (matrix-cols matrix)))) new))
(defsetf mref mref-upd)

;;; MATRIX OPERATIONS
;;; Functions for manipulating matrices: multiplication, addition, etc.
(defun * (&rest args)
  "Performs standard matrix multiplication for n x m and m x k matrices"
  ;; we define a function that multiplies two matrices
  (flet ((two-times (left right)
           (assert (= (matrix-cols left) (matrix-rows right)))
           (let ((out (make-matrix
                       :rows (matrix-rows left)
                       :cols (matrix-cols right)
                       :data (make-array (cl:* (matrix-rows left) (matrix-cols right))
                                         :initial-element 0.0 :element-type 'float))))
              (loop for i from 0 to (cl:- (matrix-rows left) 1) do
                (loop for j from 0 to (cl:- (matrix-cols right) 1) do
                  (loop for k from 0 to (cl:- (matrix-cols left) 1) do
                    (incf (mref out i j)
                          (cl:*
                           (mref left i k)
                           (mref right k j))))))
              out)))
    ;; call reduce so that '*' will generalise to any number of matrices
    (reduce #'two-times args)))

(defun + (left right)
  "Adds two matrices together"
  (declare (type matrix left right))
  (assert (and (= (matrix-cols left) (matrix-cols right))
               (= (matrix-rows left) (matrix-rows right))))
  (let ((out (make-matrix
              :rows (matrix-rows left)
              :cols (matrix-cols left)
              :data (make-array (cl:* (matrix-rows left) (matrix-cols left))
                                :initial-element 0.0))))
    (loop for i from 0 to (matrix-rows left) do
      (loop for j from 0 to (matrix-cols left) do
        (setf (mref out i j) (cl:+ (mref left i j) (mref right i j)))))))


(defun apply (matrix vector)
  "Multiplies a nxn matrix and a nx1 vector"

  ;; note the difference to the '*' function: the vector is a lisp vector, not a matrix struct!
  (declare (type matrix matrix) (type vector vector))
  (assert (= (matrix-cols matrix) (length vector)))

  (let ((result (make-array (matrix-rows matrix) :initial-element 0.0 :element-type 'float)))
    (loop for i from 0 while (< i (matrix-rows matrix)) do
      (setf (aref result i)
            (loop for j from 0 while (< j (length vector))
                  summing (cl:* (mref matrix i j) (aref vector j)) into val
                  finally (return val)))
            finally (return result))))

(defun minor (m i j)
  "Determinant of the matrix m with row i, column j removed"
  (let* ((rows (cl:- (matrix-rows m) 1))
         (cols (cl:- (matrix-cols m) 1))
         (out (make-matrix
               :rows rows
               :cols cols
               :data (make-array (cl:* rows cols) :initial-element 0.0 :element-type 'float))))
    (loop for k from 0 while (< k rows) do
      (loop for l from 0 while (< l cols) do
        ;; calculate index into the 'base' matrix
        ;; this is just adding 1 to k if k >= i, and the same for j & l
        (setf (mref out k l)
              (mref m
                    (if (>= k i) (cl:+ k 1) k)
                    (if (>= l j) (cl:+ l 1) l)))))
    (det out)))


(defun cofactor (m i j)
  "Calculates the cofactor of matrix element i j: the signed minor"
  (declare (type matrix m) (type integer i j))
  (if (evenp (cl:+ i j))
      (minor m i j)
      (cl:* -1.0 (minor m i j))))

(defun det (matrix)
  "Calculates the determinant of the matrix: returns an error if it cannot be calculated"
  (with-slots (rows cols data) matrix
    (when (not (= rows cols)) (error "Cannot calculate determinant of non-square matrix"))
    (if (and (= rows 1) (= cols 1))
        (aref data 0)
        (loop for i from 0 while (< i rows)
              summing (cl:* (mref matrix 0 i)
                            (cofactor matrix 0 i))
                into total
              finally (return total)))))


(defun inverse (matrix)
  "Calculates the inverse of a matrix, returns an error if none exists"
  (declare (type matrix matrix))
  (when (= (det matrix) 0.0) (error "Attempt to compute inverse of matrix with det 0"))
  (with-slots (rows cols data) matrix
    (let ((det (det matrix))
          (out (make-matrix
                :rows rows
                :cols cols
                :data (make-array (cl:* rows cols) :initial-element 0.0 :element-type 'float))))
      (loop for i from 0 while (< i rows) do
        (loop for j from 0 while (< j cols) do
          (setf (mref out i j) (/ (cofactor matrix j i) det))))
      out)))




;;; GENERATION FUNCTIONS
;; these functions will generate various types of matrices
(defun identity (dim)
   (let ((matrix (make-matrix
                  :rows dim
                  :cols dim
                  :data (make-array (cl:* dim dim) :initial-element 0.0 :element-type 'float))))
     (loop for i from 0 to (cl:- dim 1) do
       (setf (aref (matrix-data matrix) (cl:+ i (cl:* dim i))) 1.0))
     matrix))

(defun scale (scale)
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let ((x (elt scale 0)) (y (elt scale 1)) (z (elt scale 2)))
     (vector x 0.0 0.0 0.0
             0.0 y 0.0 0.0
             0.0 0.0 z 0.0
             0.0 0.0 0.0 1.0))))

(defun translate (pos)
  (declare (type (vector number 3) pos))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let ((x (elt pos 0)) (y (elt pos 1)) (z (elt pos 2)))
     (vector 1.0 0.0 0.0 x
             0.0 1.0 0.0 y
             0.0 0.0 1.0 z
             0.0 0.0 0.0 1.0))))

(defun rotate (angles)
  (declare (type (vector number 3) angles))
  (make-matrix 
   :rows 4
   :cols 4
   :data
   (let ((x (elt angles 0))
         (y (elt angles 1))
         (z (elt angles 2)))
     (vector (cl:* (cos z) (cos y))
             (cl:- (cl:* (cos z) (sin y) (sin x)) (cl:* (sin z) (cos x)))
             (cl:+ (cl:* (cos z) (sin y) (cos x)) (cl:* (sin z) (sin x)))
             0.0
             (cl:* (sin z) (cos y))
             (cl:+ (cl:* (sin z) (sin y) (sin x)) (cl:* (cos z) (cos x)))
             (cl:- (cl:* (sin z) (sin y) (cos x)) (cl:* (cos z) (sin x)))
             0.0
             (cl:- (sin y))
             (cl:* (cos y) (sin x))
             (cl:* (cos y) (cos x))
             0.0
             0.0
             0.0
             0.0
             1.0))))


(defun look-at (position direction up)
  (declare (type (vector number 3) position direction up))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (let* ((camera-direction direction)
          (camera-right (normalize (cross up camera-direction)))
          (camera-up (cross camera-direction camera-right)))
     (vector (elt camera-right 0)
             (elt camera-right 1)
             (elt camera-right 2)
             (dot camera-right (vec:scale -1 position))
             (elt camera-up 0)
             (elt camera-up 1)
             (elt camera-up 2)
             (dot camera-up (vec:scale -1 position))
             (elt camera-direction 0)
             (elt camera-direction 1)
             (elt camera-direction 2)
             (dot camera-direction (vec:scale -1 position))
             0.0
             0.0
             0.0
             1.0))))


(defun perspective (fov aspect near far)
  "Returns a matrix which corresponds to matrix projection"

  (declare (type number fov aspect near far))
  (assert (> fov 0.0))
  (assert (not (= aspect 0.0)))

  (make-matrix
   :rows 4
   :cols 4
   :data
   (let* ((ti (/ 1.0 (tan (/ fov 2.0))))
          (ri (cl:* ti aspect)))
     (vector ti 0.0 0.0 0.0
             0.0 ri 0.0 0.0
             0.0 0.0 (/ (cl:- (cl:+ far near)) (cl:- far near)) (/ (cl:* -2 far near) (cl:- far near))
             0.0 0.0 -1.0 0.0))))

(defun detailed-perspective (top bottom left right near far)
  "Produces a perspective transform matrix"
  (declare (type number top bottom left right near far))
  (make-matrix
   :rows 4
   :cols 4
   :data
   (vector (/ (cl:* 2 near) (cl:- right left))
           0.0
           (/ (cl:+ right left) (cl:- right left))
           0.0

           0.0
           (/ (cl:* 2 near) (cl:- top bottom))
           (/ (cl:+ top bottom) (cl:- top bottom))
           0.0

           0.0
           0.0
           (/ (cl:+ near far) (cl:- near far))
           (/ (cl:* 2 far near) (cl:- near far))

           0.0
           0.0
           -1.0
           0.0)))



