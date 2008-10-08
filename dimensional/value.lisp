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

(def generic coordinate= (coordinate-1 coordinate-2)
  (:method (coordinate-1 coordinate-2)
    (eq coordinate-1 coordinate-2))

  (:method ((coordinate-1 cons) (coordinate-2 cons))
    (if (or (coordinate-range-p coordinate-1)
            (coordinate-range-p coordinate-2))
        (coordinate-range= coordinate-1 coordinate-2)
        (and (null (set-difference coordinate-1 coordinate-2))
             (null (set-difference coordinate-2 coordinate-1)))))

  (:method ((coordinate-1 number) (coordinate-2 number))
    (= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp= coordinate-1 coordinate-2)))

(def generic coordinate< (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (< coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp< coordinate-1 coordinate-2)))

(def generic coordinate<= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (<= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp<= coordinate-1 coordinate-2)))

(def generic coordinate> (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (> coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp> coordinate-1 coordinate-2)))

(def generic coordinate>= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (>= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp>= coordinate-1 coordinate-2)))

(def function coordinate-min (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-1
      coordinate-2))

(def function coordinate-max (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-2
      coordinate-1))

;;;;;;
;;; Coordinate range

(def (type e) coordinate-range ()
  'cons)

(def (function e) coordinate-range-p (value)
  (bind ((begin (coordinate-range-begin value))
         (end (coordinate-range-end value)))
    (and (consp value)
         (not (null begin))
         (not (null end))
         (not (consp begin))
         (not (consp end)))))

(def (function io) make-coordinate-range (begin end)
  (debug-only (assert (coordinate<= begin end)))
  (cons begin end))

(def (function io) coordinate-range-begin (range)
  (car range))

(def (function io) coordinate-range-end (range)
  (cdr range))

(def (function io) coordinate-range-empty-p (range)
  (coordinate= (coordinate-range-begin range)
               (coordinate-range-end range)))

(def function in-coordinate-range-p (coordinate range)
  (and (coordinate<= (coordinate-range-begin range) coordinate)
       (coordinate<= coordinate (coordinate-range-end range))))

(def function coordinate-range= (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (and (coordinate= (coordinate-range-begin range-1)
                    (coordinate-range-begin range-2))
       (coordinate= (coordinate-range-end range-1)
                    (coordinate-range-end range-2))))

(def function assert-ranges (range-1 range-2)
  (assert (and (coordinate-range-p range-1)
               (coordinate-range-p range-2))))

(def function overlapping-range-p (range-1 range-2 &optional strict)
  (debug-only (assert-ranges range-1 range-2))
  (and (funcall (if (or (not strict)
                        (coordinate-range-empty-p range-2))
                    #'coordinate<=
                    #'coordinate<)
                (coordinate-range-begin range-1)
                (coordinate-range-end range-2))
       (funcall (if (or (not strict)
                        (coordinate-range-empty-p range-1))
                    #'coordinate<=
                    #'coordinate<)
                (coordinate-range-begin range-2)
                (coordinate-range-end range-1))))

(def function covering-range-p (cover range)
  (and (coordinate<= (coordinate-range-begin cover)
                     (coordinate-range-begin range))
       (coordinate<= (coordinate-range-end range)
                     (coordinate-range-end cover))))

(def function range-intersection (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (when (overlapping-range-p range-1 range-2 #t)
    (make-coordinate-range (coordinate-max (coordinate-range-begin range-1)
                                           (coordinate-range-begin range-2))
                           (coordinate-min (coordinate-range-end range-1)
                                           (coordinate-range-end range-2)))))

(def function range-union (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (when (overlapping-range-p range-1 range-2)
    (make-coordinate-range (coordinate-min (coordinate-range-begin range-1)
                                           (coordinate-range-begin range-2))
                           (coordinate-max (coordinate-range-end range-1)
                                           (coordinate-range-end range-2)))))

(def function range-difference (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (if (overlapping-range-p range-1 range-2 #t)
      (bind ((intersection (range-intersection range-1 range-2))
             (difference-1 (make-coordinate-range (coordinate-range-begin range-1) (coordinate-range-begin intersection)))
             (difference-2 (make-coordinate-range (coordinate-range-end intersection) (coordinate-range-end range-1)))
             (empty-1 (coordinate-range-empty-p difference-1))
             (empty-2 (coordinate-range-empty-p difference-2)))
        (cond ((and empty-1
                    empty-2)
               nil)
              (empty-1
               (list difference-2))
              (empty-2
               (list difference-1))
              (t
               (list difference-1 difference-2))))
      (list range-1)))

;;;;;;
;;; Dimension coordinate

(def (generic e) covering-coordinate-p (dimension cover coordinate)
  (:method ((dimension abstract-dimension) (cover cons) (coordinate cons))
    (every (lambda (element)
             (member element cover))
           coordinate))

  (:method ((dimension ordering-dimension) (cover cons) (coordinate cons))
    (covering-range-p cover coordinate)))

(def (generic e) coordinate-intersection (dimension coordinate-1 coordinate-2)
  (:method ((dimension abstract-dimension) (coordinate-1 null) (coordinate-2 cons))
    nil)

  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 null))
    nil)

  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (intersection coordinate-1 coordinate-2))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-intersection coordinate-1 coordinate-2)))

(def (generic e) coordinate-union (dimension coordinate-1 coordinate-2)
  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (union coordinate-1 coordinate-2))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-union coordinate-1 coordinate-2)))

(def (generic e) coordinate-difference (dimension coordinate-1 coordinate-2)
  (:method ((dimension abstract-dimension) (coordinate-1 null) (coordinate-2 null))
    nil)

  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 null))
    (list coordinate-1))

  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (list (set-difference coordinate-1 coordinate-2)))

  (:method ((dimension ordering-dimension) (coordinate-1 cons) (coordinate-2 cons))
    (range-difference coordinate-1 coordinate-2)))

;;;;;;
;;; Dimensions & coordinates

(def (function e) coordinates= (coordinates-1 coordinates-2)
  (every* #'coordinate= coordinates-1 coordinates-2))

(def (function e) coordinates-equal (coordinates-1 coordinates-2)
  (coordinates= coordinates-1 coordinates-2))

(def (function e) make-empty-coordinates (dimensions)
  (mapcar (constantly nil) dimensions))

(def (function e) covering-coordinates-p (dimensions cover-coordinates coordinates)
  (iter (for dimension :in dimensions)
        (for cover-coordinate :in cover-coordinates)
        (for coordinate :in coordinates)
        (always (covering-coordinate-p dimension cover-coordinate coordinate))))

(def (function e) coordinates-intersection (dimensions coordinates-1 coordinates-2)
  (iter (for dimension :in dimensions)
        (for coordinate-1 :in coordinates-1)
        (for coordinate-2 :in coordinates-2)
        (aif (coordinate-intersection dimension coordinate-1 coordinate-2)
             (collect it)
             (return-from coordinates-intersection nil))))

(def (function e) coordinates-union (dimensions coordinates-1 coordinates-2)
  (iter (with different-index = nil)
        (with different-dimension = nil)
        (with different-coordinate-1 = nil)
        (with different-coordinate-2 = nil)
        (for index :from 0)
        (for dimension :in dimensions)
        (for coordinate-1 :in coordinates-1)
        (for coordinate-2 :in coordinates-2)
        (when (null coordinate-1)
          (return-from coordinates-union coordinates-2))
        (when (null coordinate-2)
          (return-from coordinates-union coordinates-1))
        (unless (coordinate= coordinate-1 coordinate-2)
          (if different-index
              (return-from coordinates-union nil)
              (progn
                (setf different-index index)
                (setf different-dimension dimension)
                (setf different-coordinate-1 coordinate-1)
                (setf different-coordinate-2 coordinate-2))))
        (finally
         (return
           (if different-index
               (when-bind unified-coordinate (coordinate-union different-dimension different-coordinate-1 different-coordinate-2)
                 (aprog1 (copy-seq coordinates-1)
                   (setf (elt it different-index) unified-coordinate)))
               coordinates-1)))))

(def (function e) coordinates-difference (dimensions coordinates-1 coordinates-2)
  (if (coordinates-intersection dimensions coordinates-1 coordinates-2)
      (iter outer
            (with result-coordinates = (copy-seq coordinates-1))
            (for index :from 0)
            (for dimension :in dimensions)
            (for coordinate-1 :in coordinates-1)
            (for coordinate-2 :in coordinates-2)
            (for differences = (coordinate-difference dimension coordinate-1 coordinate-2))
            (iter (for difference :in differences)
                  (in outer (if difference
                                (collect (aprog1 (copy-seq result-coordinates)
                                           (setf (elt it index) difference)))
                                (finish))))
            (if coordinate-2
                (bind ((intersection (coordinate-intersection dimension coordinate-1 coordinate-2)))
                  (if intersection
                      (setf (elt result-coordinates index) intersection)
                      (finish)))
                (finish)))
      (list coordinates-1)))

;;;;;;
;;; C value

(def class* c-value ()
  ((coordinates)
   (value)))

(def (function e) c-value-p (value)
  (typep value 'c-value))

(def print-object c-value ()
  (princ (value-of -self-)))

(def (function e) print-c-value (c-value &optional (stream t))
  (format stream "(~{~A~^ ~}) : ~A" (coordinates-of c-value) (value-of c-value)))

(def (function e) make-c-value (coordinates value)
  (make-instance 'c-value
                 :coordinates coordinates
                 :value value))

(def (function e) copy-c-value (c-value)
  (make-instance 'c-value
                 :coordinates (coordinates-of c-value)
                 :value (value-of c-value)))

(def (function e) c-value= (c-value-1 c-value-2 &key (test #'eql))
  (and (funcall test
                (value-of c-value-1)
                (value-of c-value-2))
       (every* #'coordinates=
               (coordinates-of c-value-1)
               (coordinates-of c-value-2))))

;;;;;;
;;; D value (multi dimensional value)

(def class* d-value ()
  ((dimensions)
   (c-values)))

(def (function e) d-value-p (value)
  (typep value 'd-value))

(def print-object d-value ()
  (format t "(~{~A~^, ~})" (mapcar #'name-of (dimensions-of -self-))))

(def (function e) print-d-value (d-value &optional (stream t))
  (princ d-value stream)
  (iter (for c-value :in (c-values-of d-value))
        (terpri stream)
        (print-c-value c-value stream)))

(def function valid-d-value-p (d-value)
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value-1-cell :on (c-values-of d-value))
        (for c-value-1 = (car c-value-1-cell))
        (iter (for c-value-2 :in (cdr c-value-1-cell))
              (assert (not (coordinates-intersection dimensions (coordinates-of c-value-1) (coordinates-of c-value-2)))
                      nil "Invalid d-value due to overlapping coordinates found in c-values of ~A" d-value))))

(def (function e) make-empty-d-value (dimensions)
  (make-instance 'd-value
                 :dimensions dimensions
                 :c-values nil))

(def (function e) make-single-d-value (dimensions coordinates value)
  (make-instance 'd-value
                 :dimensions (mapcar #'lookup-dimension dimensions)
                 :c-values (list (make-c-value coordinates value))))

(def (function e) single-d-value (d-value)
  (assert (single-d-value-p d-value))
  (value-of (first (c-values-of d-value))))

(def (function e) single-d-value-p (d-value)
  (length= 1 (c-values-of d-value)))

(def (function e) copy-d-value (d-value)
  (debug-only (valid-d-value-p d-value))
  (make-instance 'd-value
                 :dimensions (dimensions-of d-value)
                 :c-values (mapcar #'copy-c-value (c-values-of d-value))))

(def (function e) d-value= (d-value-1 d-value-2 &key (test #'eql))
  (debug-only (and (valid-d-value-p d-value-1)
                   (valid-d-value-p d-value-2)))
  (and (equal (dimensions-of d-value-1)
              (dimensions-of d-value-2))
       (every* (lambda (c-value-1 c-value-2)
                 (c-value= c-value-1 c-value-2 :test test))
               (c-values-of d-value-1)
               (c-values-of d-value-2))))

(def (function e) d-value-equal (d-value-1 d-value-2 &key (test #'eql))
  (iter (with dimensions-1 = (dimensions-of d-value-1))
        (with dimensions-2 = (dimensions-of d-value-1))
        (unless (equal dimensions-1 dimensions-2)
          (return-from d-value-equal #f))
        (for c-value-1 :in (c-values-of d-value-1))
        (for d-value-1-part = (make-single-d-value dimensions-1 (coordinates-of c-value-1) (value-of c-value-1)))
        (for d-value-2-part = (value-at-coordinates d-value-2 (coordinates-of c-value-1)))
        (always (or (d-value= d-value-1-part
                              d-value-2-part
                              :test test)
                    (d-value-equal d-value-2-part
                                   d-value-1-part
                                   :test test)))))

(def (function e) covering-d-value-p (d-value coordinates)
  (debug-only (valid-d-value-p d-value))
  (bind ((remaining-coordinates (list coordinates)))
    (iter (with dimensions = (dimensions-of d-value))
          (for c-value :in (c-values-of d-value))
          (setf remaining-coordinates
                (iter (for remaining-coordinate :in remaining-coordinates)
                      (appending (coordinates-difference dimensions remaining-coordinate (coordinates-of c-value))))))
    (null remaining-coordinates)))

(def (function e) consolidate-d-value (d-value &key (test #'eql))
  (debug-only (valid-d-value-p d-value))
  (tagbody
   :restart
     (iter outer
           (with dimensions = (dimensions-of d-value))
           (for c-value-1-cell :on (c-values-of d-value))
           (for c-value-1 = (car c-value-1-cell))
           (unless (iter inner
                         (for c-value-2 :in (cdr c-value-1-cell))
                         (when (funcall test
                                        (value-of c-value-1)
                                        (value-of c-value-2))
                           (when-bind coordinates (coordinates-union dimensions (coordinates-of c-value-1) (coordinates-of c-value-2))
                             (setf (c-values-of d-value)
                                   (list* (make-c-value coordinates (value-of c-value-1))
                                          (remove-if (lambda (c-value)
                                                       (or (eq c-value c-value-1)
                                                           (eq c-value c-value-2)))
                                                     (c-values-of d-value))))
                             (go :restart))))
             (collect c-value-1))))
  (debug-only (valid-d-value-p d-value))
  d-value)

(def (function e) single-value-at-coordinates (d-value coordinates &key (otherwise nil otherwise?))
  (debug-only (valid-d-value-p d-value))
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value :in (c-values-of d-value))
        (when (covering-coordinates-p dimensions (coordinates-of c-value) coordinates)
          (return-from single-value-at-coordinates (value-of c-value)))
        (finally
         (handle-otherwise
          (if otherwise?
              otherwise
              (list :error "Covering c-value not found for ~A in ~A" coordinates d-value))))))

(def (function e) value-at-coordinates (d-value coordinates)
  (debug-only (valid-d-value-p d-value))
  (consolidate-d-value
   (prog1-bind result-d-value (make-empty-d-value (dimensions-of d-value))
     (setf (c-values-of result-d-value)
           (iter (with dimensions = (dimensions-of d-value))
                 (for c-value :in (c-values-of d-value))
                 (collect (make-c-value (coordinates-intersection dimensions (coordinates-of c-value) coordinates)
                                        (value-of c-value))))))))

(def (function e) (setf value-at-coordinates) (new-value d-value coordinates)
  (debug-only (valid-d-value-p d-value))
  (setf (c-values-of d-value)
        (list* (make-c-value coordinates new-value)
               (iter (with dimensions = (dimensions-of d-value))
                     (for c-value :in (c-values-of d-value))
                     (mapc (lambda (coordinates)
                             (collect (make-c-value coordinates (value-of c-value))))
                           (coordinates-difference dimensions (coordinates-of c-value) coordinates)))))
  (consolidate-d-value d-value))

(def function d-values-have-same-dimensions-p (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (every (lambda (d-value)
             (equal dimensions
                    (dimensions-of d-value)))
           d-values)))

;;;;;;
;;; Iteration support

(defmacro-clause (for variables :in-d-value d-value)
  (assert (length= 2 variables))
  (with-unique-names (d-value-variable dimensions-variable one-dimensional-variable? c-value-variable coordinates-variable)
    `(progn
       (with ,d-value-variable = ,d-value)
       (with ,dimensions-variable = (dimensions-of ,d-value-variable))
       (with ,one-dimensional-variable? = (length= 1 ,dimensions-variable))
       (for ,c-value-variable :in (c-values-of ,d-value-variable))
       (for ,coordinates-variable = (coordinates-of ,c-value-variable))
       (for ,(first variables) = (if ,one-dimensional-variable?
                                     (first ,coordinates-variable)
                                     ,coordinates-variable))
       (for ,(second variables) = (value-of ,c-value-variable)))))

(defmacro-clause (for variables :in-d-values d-values)
  (with-unique-names (d-values-variable dimensions-variable one-dimensional-variable? coordinates-variable)
    `(progn
       (with ,d-values-variable = (list ,@d-values))
       (with ,dimensions-variable = (dimensions-of (first ,d-values-variable)))
       (with ,one-dimensional-variable? = (length= 1 ,dimensions-variable))
       (for ,coordinates-variable :in (split-d-values-coordinates-lists ,d-values-variable))
       (for ,(first variables) = (if ,one-dimensional-variable?
                                     (first ,coordinates-variable)
                                     ,coordinates-variable))
       (for ,(cdr variables) = (mapcar (lambda (d-value)
                                         (single-value-at-coordinates d-value ,coordinates-variable :otherwise :unspecified-value))
                                       ,d-values-variable)))))

(iter::defclause (collect-d-value expr &optional dimensions dimensions coordinates coordinates into variable)
  (bind ((collect-variable-spec (or variable iter::*result-var*))
         (collect-variable (iter::extract-var collect-variable-spec)))
    (iter::make-accum-var-binding collect-variable-spec nil :collect-d-value)
    (iter::return-code
     :initial `((setf ,collect-variable (make-empty-d-value ,dimensions)))
     :body `((progn
               (setf (value-at-coordinates ,collect-variable ,coordinates) ,expr))))))

(export 'collect-d-value)

(def (function e) map-d-values (function d-values &key unspecified-value)
  (assert (d-values-have-same-dimensions-p d-values))
  (mapc (lambda (coordinates)
          (apply function
                 coordinates
                 (mapcar (lambda (d-value)
                           (single-value-at-coordinates d-value coordinates :otherwise unspecified-value))
                         d-values)))
        (split-d-values-coordinates-lists d-values)))

(def function coordinates-list-difference (dimensions coordinates-list-1 coordinates-list-2)
  (iter outer
        (for coordinates-1 :in coordinates-list-1)
        (for differences = (iter (for coordinates-2 :in coordinates-list-2)
                                 (collect (coordinates-difference dimensions coordinates-1 coordinates-2))))
        (when differences
          (appending (reduce (lambda (coordinates-list-1 coordinates-list-2)
                               (coordinates-list-intersection dimensions coordinates-list-1 coordinates-list-2))
                             differences)))))

(def function coordinates-list-intersection (dimensions coordinates-list-1 coordinates-list-2)
  (iter outer
        (for coordinates-1 :in coordinates-list-1)
        (iter (for coordinates-2 :in coordinates-list-2)
              (awhen (coordinates-intersection dimensions coordinates-1 coordinates-2)
                (in outer (collect it))))))

(def function split-d-values-coordinates-lists (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (reduce (lambda (coordinates-list-1 coordinates-list-2)
              (split-coordinates-lists dimensions coordinates-list-1 coordinates-list-2))
            d-values
            :key (lambda (d-value)
                   (mapcar #'coordinates-of (c-values-of d-value))))))

(def function split-coordinates-lists (dimensions coordinates-list-1 coordinates-list-2)
  (bind ((intersections (coordinates-list-intersection dimensions coordinates-list-1 coordinates-list-2)))
    (append intersections
            (coordinates-list-difference dimensions coordinates-list-1 intersections)
            (coordinates-list-difference dimensions coordinates-list-2 intersections))))
