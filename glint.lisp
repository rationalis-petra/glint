(defpackage :glint
  (:use :cl :glint.core :glint.graphics :glint.physics)
  (:export
   :default-update-args :delta-time :make-delta-timer :cursor-del :make-cursor-del))

(in-package :glint)

(defclass delta-time () 
  ((dt
    :type number
    :accessor delta-time
    :initform 0)))

(defun make-delta-timer ()
  "Make a function whose return value is the the difference between the now and
the time of its' most recent call"
  (let ((prev-time (get-internal-real-time)))
    (lambda (update-args)
      (let ((curr-time (get-internal-real-time)))
        (setf (delta-time update-args)
              (/ (- curr-time prev-time)
                 internal-time-units-per-second))
        (setf prev-time curr-time)))))

(defclass cursor-dpos () 
  ((delta-cursor-pos
    :type (vector number 2)
    :accessor cursor-del
    :initform (vector 0 0))))

(defun make-cursor-del ()
  "Make a function whose return value is the the difference between the now and
the time of its' most recent call"
  (let ((prev-pos (get-cursor-pos)))
    (lambda (update-args)
      (let ((curr-pos (get-cursor-pos)))
        (setf (cursor-del update-args)
              (vec:- curr-pos prev-pos))
        (setf prev-pos curr-pos)))))

(defun make-simple-engine ()
  (make-instance 'engine
                 :tickers (list #'update)
                 :update-arg-tickers (list (make-delta-timer)
                                           (make-cursor-del))))

(defclass default-update-args (delta-time cursor-dpos) ())
