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

(def (generic e) coordinate= (coordinate-1 coordinate-2)
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

(def (generic e) coordinate< (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (< coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp< coordinate-1 coordinate-2)))

(def (generic e) coordinate<= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (<= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp<= coordinate-1 coordinate-2)))

(def (generic e) coordinate> (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (> coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp> coordinate-1 coordinate-2)))

(def (generic e) coordinate>= (coordinate-1 coordinate-2)
  (:method ((coordinate-1 number) (coordinate-2 number))
    (>= coordinate-1 coordinate-2))

  (:method ((coordinate-1 timestamp) (coordinate-2 timestamp))
    (timestamp>= coordinate-1 coordinate-2)))

(def (function e) coordinate-min (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-1
      coordinate-2))

(def (function e) coordinate-max (coordinate-1 coordinate-2)
  (if (coordinate< coordinate-1 coordinate-2)
      coordinate-2
      coordinate-1))

;;;;;;
;;; Coordinate range

(def (type e) bounds ()
  '(member ii ie ei ee))

(export '(ii ie ei ee))

(def (function ioe) make-bounds (begin-inclusive end-inclusive)
  (cond
    ((and begin-inclusive end-inclusive) 'ii)
    (begin-inclusive 'ie)
    (end-inclusive 'ei)
    (t 'ee)))

(def (function ioe) begin-inclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ii ie)))

(def (function ioe) begin-exclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ei ee)))

(def (function ioe) end-inclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ii ei)))

(def (function ioe) end-exclusive-p (bounds)
  (debug-only (assert (typep bounds 'bounds)))
  (member bounds '(ie ee)))

(def (type e) coordinate-range ()
  '(cons bounds (cons coordinate coordinate)))

(def (function e) coordinate-range-p (value)
  (and (consp value)
       (consp (cdr value))
       (bind ((bounds (coordinate-range-bounds value))
              (begin (coordinate-range-begin value))
              (end (coordinate-range-end value)))
         (and (typep bounds 'bounds)
              (not (null begin))
              (not (null end))
              (not (consp begin))
              (not (consp end))))))

(def (function ioe) make-coordinate-range (bounds begin end)
  (debug-only
    (assert (typep bounds 'bounds))
    (assert (coordinate<= begin end))
    (assert (or (coordinate< begin end) (eq bounds 'ii))))
  (cons bounds (cons begin end)))

(def (function io) make-coordinate-range* (bounds begin end)
  (when (or (coordinate< begin end)
            (and (coordinate= begin end)
                 (eq bounds 'ii)))
    (debug-only (assert (typep bounds 'bounds)))
    (cons bounds (cons begin end))))

(def (function ioe) coordinate-range-bounds (range)
  (car range))

(def (function ioe) coordinate-range-begin (range)
  (cadr range))

(def (function ioe) coordinate-range-end (range)
  (cddr range))

(def (function ioe) make-empty-coordinate-range (coordinate)
  (cons 'ii (cons coordinate coordinate)))

(def (function ioe) coordinate-range-empty-p (range)
  (and (eq 'ii (coordinate-range-bounds range))
       (coordinate= (coordinate-range-begin range)
                (coordinate-range-end range))))

(def function in-coordinate-range-p (coordinate range)
  (ecase (coordinate-range-bounds range)
    (ii (and (coordinate<= (coordinate-range-begin range) coordinate)
             (coordinate<= coordinate (coordinate-range-end range))))
    (ie (and (coordinate<= (coordinate-range-begin range) coordinate)
             (coordinate< coordinate (coordinate-range-end range))))
    (ei (and (coordinate< (coordinate-range-begin range) coordinate)
             (coordinate<= coordinate (coordinate-range-end range))))
    (ee (and (coordinate< (coordinate-range-begin range) coordinate)
             (coordinate< coordinate (coordinate-range-end range))))))

(def function coordinate-range= (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (and (eq (coordinate-range-bounds range-1)
           (coordinate-range-bounds range-2))
       (coordinate= (coordinate-range-begin range-1)
                    (coordinate-range-begin range-2))
       (coordinate= (coordinate-range-end range-1)
                    (coordinate-range-end range-2))))

(def function assert-ranges (range-1 range-2)
  (assert (and (coordinate-range-p range-1)
               (coordinate-range-p range-2))))

(def (function e) overlapping-range-p (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (bind ((bounds-1 (coordinate-range-bounds range-1))
         (bounds-2 (coordinate-range-bounds range-2)))
    (and (funcall (if (and (begin-inclusive-p bounds-1)
                           (end-inclusive-p bounds-2))
                     #'coordinate<=
                     #'coordinate<)
                 (coordinate-range-begin range-1)
                 (coordinate-range-end range-2))
         (funcall (if (and (end-inclusive-p bounds-1)
                           (begin-inclusive-p bounds-2))
                     #'coordinate<=
                     #'coordinate<)
                 (coordinate-range-begin range-2)
                 (coordinate-range-end range-1)))))

(def function covering-range-p (cover range)
  (debug-only (assert-ranges cover range))
  (bind ((cover-bounds (coordinate-range-bounds cover))
         (range-bounds (coordinate-range-bounds range)))
    (and (funcall (if (and (begin-exclusive-p cover-bounds)
                           (begin-inclusive-p range-bounds))
                      #'coordinate<
                      #'coordinate<=)
                  (coordinate-range-begin cover)
                  (coordinate-range-begin range))
         (funcall (if (and (end-exclusive-p cover-bounds)
                           (end-inclusive-p range-bounds))
                      #'coordinate<
                      #'coordinate<=)
                  (coordinate-range-end range)
                  (coordinate-range-end cover)))))

(def function range-intersection (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (when (overlapping-range-p range-1 range-2)
    (bind ((begin-range (cond
                          ((coordinate< (coordinate-range-begin range-1)
                                        (coordinate-range-begin range-2))
                           range-2)
                          ((coordinate< (coordinate-range-begin range-2)
                                        (coordinate-range-begin range-1))
                           range-1)
                          ((begin-exclusive-p (coordinate-range-bounds range-1))
                           range-1)
                          (t
                           range-2)))
           (end-range (cond
                        ((coordinate< (coordinate-range-end range-1)
                                      (coordinate-range-end range-2))
                         range-1)
                        ((coordinate< (coordinate-range-end range-2)
                                      (coordinate-range-end range-1))
                         range-2)
                        ((end-exclusive-p (coordinate-range-bounds range-1))
                         range-1)
                        (t
                         range-2))))
      (make-coordinate-range
       (make-bounds (begin-inclusive-p (coordinate-range-bounds begin-range))
                    (end-inclusive-p (coordinate-range-bounds end-range)))
       (coordinate-range-begin begin-range)
       (coordinate-range-end end-range)))))

(def function range-union (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (when (or (overlapping-range-p range-1 range-2)
            (and (coordinate= (coordinate-range-end range-1)
                              (coordinate-range-begin range-2))
                 (or (end-inclusive-p (coordinate-range-bounds range-1))
                     (begin-inclusive-p (coordinate-range-bounds range-2))))
            (and (coordinate= (coordinate-range-end range-2)
                              (coordinate-range-begin range-1))
                 (or (end-inclusive-p (coordinate-range-bounds range-2))
                     (begin-inclusive-p (coordinate-range-bounds range-1)))))
    (bind ((begin-range (cond
                          ((coordinate< (coordinate-range-begin range-1)
                                        (coordinate-range-begin range-2))
                           range-1)
                          ((coordinate< (coordinate-range-begin range-2)
                                        (coordinate-range-begin range-1))
                           range-2)
                          ((begin-inclusive-p (coordinate-range-bounds range-1))
                           range-1)
                          (t
                           range-2)))
           (end-range (cond
                        ((coordinate< (coordinate-range-end range-1)
                                      (coordinate-range-end range-2))
                         range-2)
                        ((coordinate< (coordinate-range-end range-2)
                                      (coordinate-range-end range-1))
                         range-1)
                        ((end-inclusive-p (coordinate-range-bounds range-1))
                         range-1)
                        (t
                         range-2))))
      (make-coordinate-range
       (make-bounds (begin-inclusive-p (coordinate-range-bounds begin-range))
                    (end-inclusive-p (coordinate-range-bounds end-range)))
       (coordinate-range-begin begin-range)
       (coordinate-range-end end-range)))))

(def function range-difference (range-1 range-2)
  (debug-only (assert-ranges range-1 range-2))
  (cond
    ((covering-range-p range-2 range-1)
     nil)
    ((overlapping-range-p range-1 range-2)
     (bind ((intersection (range-intersection range-1 range-2))
            (difference-1 (make-coordinate-range*
                           (make-bounds (begin-inclusive-p (coordinate-range-bounds range-1))
                                        (begin-exclusive-p (coordinate-range-bounds intersection)))
                           (coordinate-range-begin range-1)
                           (coordinate-range-begin intersection)))
            (difference-2 (make-coordinate-range*
                           (make-bounds (end-exclusive-p (coordinate-range-bounds intersection))
                                        (end-inclusive-p (coordinate-range-bounds range-1)))
                           (coordinate-range-end intersection)
                           (coordinate-range-end range-1))))
       (cond ((and difference-1 difference-2)
              (list difference-1 difference-2))
             (difference-1
              (list difference-1))
             (difference-2
              (list difference-2))
             (t
              nil))))
    (t (list range-1))))

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
  (:method ((dimension abstract-dimension) (coordinate-1 null) (coordinate-2 cons))
    coordinate-2)

  (:method ((dimension abstract-dimension) (coordinate-1 cons) (coordinate-2 null))
    coordinate-1)
  
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

;; FIXME ???
(def (function e) make-empty-coordinates (dimensions)
  (mapcar (constantly nil) dimensions))

(def (function e) collect-subcoordinates (dimensions sub-dimensions coordinates)
  (iter (for dimension :in dimensions)
        (for coordinate :in coordinates)
        (when (member dimension sub-dimensions)
          (collect coordinate))))

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

;; TODO
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
       (coordinates= (coordinates-of c-value-1)
                     (coordinates-of c-value-2))))

;;;;;;
;;; D value (multi dimensional value)

(def class* d-value ()
  ((dimensions)
   (c-values)))

(def (function e) d-value-p (value)
  (typep value 'd-value))

(def print-object d-value ()
  (format t "(~{~A~^, ~}) " (mapcar #'name-of (dimensions-of -self-)))
  (mapc #'print-c-value (c-values-of -self-)))

(def (function e) print-d-value (d-value &optional (stream t))
  (princ d-value stream)
  (iter (for c-value :in (c-values-of d-value))
        (terpri stream)
        (print-c-value c-value stream)))

(def function valid-d-value-p (d-value)
  (iter (with dimensions = (dimensions-of d-value))
        (with number-of-dimensions = (length dimensions))
        (for c-value-1-cell :on (c-values-of d-value))
        (for c-value-1 = (car c-value-1-cell))
        (assert (length= number-of-dimensions (length (coordinates-of c-value-1)))
                nil "Invalid number of coordinates in ~A" d-value)
        (iter (for c-value-2 :in (cdr c-value-1-cell))
              (assert (not (coordinates-intersection dimensions (coordinates-of c-value-1) (coordinates-of c-value-2)))
                      nil "Invalid d-value due to overlapping coordinates found in c-values of ~A" d-value))))

(def (function e) make-empty-d-value (dimensions)
  (make-instance 'd-value
                 :dimensions (mapcar #'lookup-dimension dimensions)
                 :c-values nil))

(def (function e) make-single-d-value (dimensions coordinates value)
  (make-instance 'd-value
                 :dimensions (mapcar #'lookup-dimension dimensions)
                 :c-values (list (make-c-value coordinates value))))

(def (function e) make-d-value (dimensions coordinates-list values)
  (prog1-bind d-value
      (make-empty-d-value dimensions)
    (mapc (lambda (coordinates value)
            (setf (value-at-coordinates d-value coordinates) value))
          coordinates-list values)))

(def (function e) single-d-value (d-value)
  (assert (single-d-value-p d-value))
  (value-of (first (c-values-of d-value))))

(def (function e) empty-d-value-p (d-value)
  (null (c-values-of d-value)))

(def (function e) single-d-value-p (d-value)
  (length= 1 (c-values-of d-value)))

(def (function e) d-values-have-same-dimensions-p (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (every (lambda (d-value)
             (equal dimensions
                    (dimensions-of d-value)))
           d-values)))

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
  (debug-only
    (valid-d-value-p d-value-1)
    (valid-d-value-p d-value-2))
  (iter (with dimensions-1 = (dimensions-of d-value-1))
        (with dimensions-2 = (dimensions-of d-value-2))
        (unless (equal dimensions-1 dimensions-2)
          (return-from d-value-equal #f))
        (for c-value-1 :in (c-values-of d-value-1))
        (for d-value-1-part = (make-single-d-value dimensions-1 (coordinates-of c-value-1) (value-of c-value-1)))
        (for d-value-2-part = (value-at-coordinates d-value-2 (coordinates-of c-value-1)))
        (always (or (d-value= d-value-1-part
                              d-value-2-part
                              :test test)
                    (and
                     (not (single-d-value-p d-value-2-part))
                     (d-value-equal d-value-2-part
                                    d-value-1-part
                                    :test test))))))

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
  (bind ((original-d-value (debug-only (copy-d-value d-value))))
    (declare (ignorable original-d-value))
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
    #+nil
    (debug-only (and (d-value-equal d-value original-d-value)
                     (valid-d-value-p d-value)))
    d-value))

(def (function e) single-value-at-coordinates (d-value coordinates &key (otherwise :signal-default-error))
  (debug-only (valid-d-value-p d-value))
  (iter (with dimensions = (dimensions-of d-value))
        (for c-value :in (c-values-of d-value))
        (when (covering-coordinates-p dimensions (coordinates-of c-value) coordinates)
          (return-from single-value-at-coordinates (value-of c-value)))
        (finally
         (return (handle-otherwise
                  (if (eq otherwise :signal-default-error)
                      (list :error "Covering c-value not found for ~A in ~A" coordinates d-value)
                      otherwise))))))

(def (function e) value-at-coordinates (d-value coordinates)
  (debug-only (valid-d-value-p d-value))
  (consolidate-d-value
   (prog1-bind result-d-value (make-empty-d-value (dimensions-of d-value))
     (setf (c-values-of result-d-value)
           (iter (with dimensions = (dimensions-of d-value))
                 (for c-value :in (c-values-of d-value))
                 (for intersection = (coordinates-intersection dimensions (coordinates-of c-value) coordinates))
                 (when intersection
                   (collect (make-c-value intersection
                                          (value-of c-value)))))))))

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

(def (function e) (setf into-d-value) (new-d-value d-value)
  (debug-only (and (valid-d-value-p new-d-value)
                   (valid-d-value-p d-value)))
  (iter (for c-value :in (c-values-of new-d-value))
        (setf (value-at-coordinates d-value (coordinates-of c-value))
              (value-of c-value))))

(def (function e) insert-at-coordinates (d-value coordinates value)
  (debug-only (valid-d-value-p d-value))
  ;; TODO: this is suboptimal
  (bind ((new-d-value (value-at-coordinates d-value coordinates)))
    (iter (for c-value :in (c-values-of new-d-value))
          (pushnew value (value-of c-value)))
    (setf (into-d-value d-value) new-d-value)))

(def (function e) delete-at-coordinates (d-value coordinates value)
  (debug-only (valid-d-value-p d-value))
  (bind ((new-d-value (value-at-coordinates d-value coordinates)))
    (iter (for c-value :in (c-values-of new-d-value))
          (deletef (value-of c-value) value))
    (setf (into-d-value d-value) new-d-value)))

(def (function e) clear-at-coordinates (d-value coordinates value)
  (debug-only (valid-d-value-p d-value))
  (bind ((new-d-value (value-at-coordinates d-value coordinates)))
    (iter (for c-value :in (c-values-of new-d-value))
          (when (eq (value-of c-value) value)
            (setf (value-of c-value) nil)))
    (setf (into-d-value d-value) new-d-value)))

;;;;;;
;;; Iteration support

(defmacro-clause (for variables :in-d-value d-value)
  (assert (length= 2 variables))
  (with-unique-names (d-value-variable c-value-variable)
    `(progn
       (with ,d-value-variable = ,d-value)
       (for ,c-value-variable :in (c-values-of ,d-value-variable))
       (for ,(first variables) = (coordinates-of ,c-value-variable))
       (for ,(second variables) = (value-of ,c-value-variable)))))

(defmacro-clause (for variables :in-d-values d-values :unspecified-value unspecified-value)
  (with-unique-names (d-values-variable coordinates-variable)
    `(progn
       (with ,d-values-variable = ,(if (listp d-values)
                                       `(list ,@d-values)
                                       d-values))
       (for ,coordinates-variable :in (split-d-values-coordinates-lists ,d-values-variable))
       (for ,(first variables) = ,coordinates-variable)
       (for ,(second variables) = (mapcar (lambda (d-value)
                                            (single-value-at-coordinates d-value ,coordinates-variable :otherwise ,unspecified-value))
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

(def (function e) map-d-value (d-value function)
  (mapc (lambda (c-value)
          (funcall function (coordinates-of c-value) (value-of c-value)))
        (c-values-of d-value)))

(def (function e) mapcar-d-value (d-value function)
  (mapcar (lambda (c-value)
            (funcall function (coordinates-of c-value) (value-of c-value)))
          (c-values-of d-value)))

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
  (when (null coordinates-list-2)
    (return-from coordinates-list-difference coordinates-list-1))
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

(def function split-coordinates-lists (dimensions coordinates-list-1 coordinates-list-2)
  (bind ((intersections (coordinates-list-intersection dimensions coordinates-list-1 coordinates-list-2)))
    (append intersections
            (coordinates-list-difference dimensions coordinates-list-1 intersections)
            (coordinates-list-difference dimensions coordinates-list-2 intersections))))

(def function split-d-values-coordinates-lists (d-values)
  (bind ((dimensions (dimensions-of (first d-values))))
    (reduce (lambda (coordinates-list-1 coordinates-list-2)
              (split-coordinates-lists dimensions coordinates-list-1 coordinates-list-2))
            d-values
            :key (lambda (d-value)
                   (mapcar #'coordinates-of (c-values-of d-value))))))

;;;;;;
;;; D operations

(def (function e) d-apply (function d-values &key (unspecified-value :signal-default-error))
  (assert (d-values-have-same-dimensions-p d-values))
  (iter (with dimensions = (dimensions-of (first d-values)))
        (for (coordinates values) :in-d-values d-values :unspecified-value unspecified-value)
        (collect-d-value (apply function values)
                         :dimensions dimensions
                         :coordinates (if (length= 1 dimensions)
                                          (list coordinates)
                                          coordinates))))

(def (function e) d-project (function projection-dimensions d-value)
  (bind ((dimensions (dimensions-of d-value))
         (remaining-dimensions (set-difference dimensions projection-dimensions))
         (projection-coordinates-list (remove-duplicates (mapcar-d-value d-value
                                                                         (lambda (coordinates value)
                                                                           (declare (ignorable value))
                                                                           (collect-subcoordinates dimensions projection-dimensions coordinates)))
                                                         :test #'coordinates=)))
    (setf projection-coordinates-list
          (coordinates-list-intersection projection-dimensions
                                         projection-coordinates-list
                                         projection-coordinates-list))
    (iter (for projection-coordinates :in projection-coordinates-list)
          (for coordinates = (iter (generate projection-coordinate :in projection-coordinates)
                                   (for dimension :in dimensions)
                                   (collect (if (member dimension projection-dimensions)
                                                (next projection-coordinate)
                                                (cons (minimum-coordinate-of dimension) (maximum-coordinate-of dimension))))))
          (for projected-d-value = (value-at-coordinates d-value coordinates))
          (collect-d-value (apply function remaining-dimensions
                                  (iter (for c-value :in (c-values-of projected-d-value))
                                        (for value = (value-of c-value))
                                        (for coordinates = (collect-subcoordinates dimensions remaining-dimensions (coordinates-of c-value)))
                                        (when coordinates
                                          (collect value :into values)
                                          (collect coordinates :into remaining-coordinates-list))
                                        (finally
                                         (return (list remaining-coordinates-list values)))))
                           :dimensions projection-dimensions
                           :coordinates projection-coordinates))))

(def (function e) d-equal (d-value-1 d-value-2)
  (d-apply #'equal (list d-value-1 d-value-2)))

(def (function e) d= (d-value &rest d-values)
  (d-apply #'= (list* d-value d-values)))

(def (function e) d+ (&rest d-values)
  (d-apply #'+ d-values))

(def (function e) d- (d-value &rest d-values)
  (d-apply #'- (list* d-value d-values)))

(def (function e) d* (&rest d-values)
  (d-apply #'* d-values))

(def (function e) d/ (d-value &rest d-values)
  (d-apply #'/ (list* d-value d-values)))

(def (macro e) d-incf (place delta)
  `(setf ,place (d+ ,place ,delta)))

(def (macro e) d-decf (place delta)
  `(setf ,place (d+ ,place ,delta)))
