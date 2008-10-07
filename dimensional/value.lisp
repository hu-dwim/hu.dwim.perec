;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Coordinate

(def (type e) coordinate ()
  t)

(def (function e) coordinate-p (value)
  (declare (ignore value))
  #t)

(def (type e) coordinate-range ()
  'cons)

(def (function e) coordinate-range-p (value)
  (consp value))

;;;;;;
;;; D value (multi dimensional value)

(def class* d-value ()
  ((dimensions)
   (coordinates)
   (values)))

(def print-object d-value ()
  (format t "(~{~A~^, ~})" (mapcar #'name-of (d-value-dimensions -self-))))

(def (function e) print-d-value (value &optional stream)
  #+nil
  (iter (with dimensions = (d-value-dimensions value))
        (for (value &rest coordinates) :in-d-values value)
        (format stream "(")
        (iter (for dimension :in dimensions)
              (for coordinate :in coordinates)
              (unless (first-iteration-p)
                (format stream ", "))
              (format stream "~A : ~A" (name-of dimension) coordinate))
        (format stream ") : ~A" value)))

(def (function e) d-value-p (value)
  (typep value 'd-value))

(def (function e) d-value-dimensions (value)
  (labels ((recurse (value)
             (bind ((inner-value (first (values-of value))))
               (if (d-value-p inner-value)
                   (append (dimensions-of value)
                           (recurse inner-value))
                   (dimensions-of value)))))
    (recurse value)))

(def (function e) make-empty-d-value ()
  (make-instance 'd-value
                 :dimensions nil
                 :coordinates nil
                 :values nil))

(def (function e) make-single-d-value (dimensions &key coordinates value)
  (make-instance 'd-value
                 :dimensions (mapcar #'lookup-dimension dimensions)
                 :coordinates coordinates
                 :values (list value)))

(def (function e) single-d-value (value)
  (assert (single-d-value-p value))
  (first (values-of value)))

(def (function e) single-d-value-p (value)
  (bind ((values (values-of value))
         (inner-value (first values)))
    (and (length= 1 values)
         (not (d-value-p inner-value)))))

(def (function e) consolidate-d-value (value &key (test #'eql))
  ;; TODO:
  )

(def (function e) d-value= (value-1 value-2)
  )

(def (function e) value-at-coordinates (d-value coordinates)
  )

(def (function e) (setf value-at-coordinates) (new-value value coordinates)
  )

;;;;;;
;;; Iteration support

(defmacro-clause (for dimensions-and-value :in-d-values d-values)
  )

;; use it like (iter (for (x y value) :in-d-values ((x-of z) (y-of u))))

(iter::defclause (collect-d-value expr
                                  &optional
                                  coordinates coordinates
                                  into variable)
  )

;;(for x = (make-d-value dimensions))
;;(collect-d-value (+ a b) :coordinates coordinates :into x)

(def function map-d-values (function d-values)
  ;; TODO:
  )

;;;;;;
;;; Examples

#|
(make-instance 'd-value
               :dimensions '(x y)
               :coordinates '((0 100) (0 100))
               :values (list 1))

(make-instance 'd-value
               :dimensions '(x)
               :coordinates '((0 100 200 300))
               :values (list (make-instance 'd-value
                                            :dimensions '(y)
                                            :coordinates '((0 20))
                                            :value '(1))
                             (make-instance 'd-value
                                            :dimensions '(y)
                                            :coordinates '((0 10 30))
                                            :value '(3 2))
                             (make-instance 'd-value
                                            :dimensions '(y)
                                            :coordinates '((0 10))
                                            :value '(3))))
|#
