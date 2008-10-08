;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;
;;; Invariants:
;;;
;;;   - Only one dimension can be inheriting-dimension.

;;;
;;; Slot access
;;;
(def method restore-slot ((d-class persistent-class-d) (d-instance d-object) (d-slot persistent-effective-slot-definition-d) &key coordinates)
  (bind (((d-slot-default-value . has-default-p) (default-value-for-type-of d-slot))
         (default-value (if has-default-p d-slot-default-value +unbound-slot-marker+))
         (dimensions (dimensions-of d-slot))
         (records (select-slot-values-with-dimensions d-class d-instance d-slot coordinates)))
    (collect-values-having-dimensions
     records
     default-value
     dimensions
     coordinates)))

;;;
;;; d-value builders
;;;
(def function collect-values-having-dimensions (records default-value dimensions coordinates)
  (prog1-bind d-value (make-single-d-value dimensions coordinates default-value)
    (iter (for record :in records)
          (for value = (first records))
          (for coords = (coordinates-intersection (rest records) coordinates))
          (setf (value-at-coordinates d-value coords) value))))

;;;
;;; Queries
;;;
(def function dependent-object-slot-names (dimension)
  (mapcar #'slot-definition-name
          (class-direct-slots (find-class (dependent-object-name-of dimension)))))

(def (function io) slot-name-of (dimension)
  (assert (or (not (typep dimension 'ordering-dimension)) (typep dimension 'inheriting-dimension)))
  (name-of dimension))

(def (function io) begin-slot-name-of (dimension)
  (assert (and (typep dimension 'ordering-dimension) (not (typep dimension 'inheriting-dimension))))
  (format-symbol "~A-BEGIN" (symbol-package (name-of dimension)) (name-of dimension)))

(def (function io) end-slot-name-of (dimension)
  (assert (and (typep dimension 'ordering-dimension) (not (typep dimension 'inheriting-dimension))))
  (format-symbol "~A-END" (symbol-package (name-of dimension)) (name-of dimension)))

(def function slot-names-of (dimension)
  (typecase dimension
    (inheriting-dimension (list (slot-name-of dimension)))
    (ordering-dimension (list (begin-slot-name-of dimension) (end-slot-name-of dimension)))
    (dimension (list (slot-name-of dimension)))))

(def (function io) dimension-equal (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp=)
    (t 'equal)))

(def function dimension-less (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp<)
    ;; TODO string<
    (t '<)))

(def function dimension-less-or-equal (dimension)
  (case (the-type-of dimension)
    ((time date timestamp) 'timestamp<=)
    ;; TODO string<=
    (t '<=)))

(def function select-slot-values-with-dimensions (d-class d-instance d-slot coordinates)
  "Returns the values of the slot and the coordinate values. The records are ordered by the coordinates with inherited dimension. When there are inherited dimensions, only the most recent returned. A coordinate is either a (begin . end) pair (ordered dimension) or a set (enumerated dimension) ."
  (bind ((h-class-name (class-name (h-class-of d-class)))
         (h-slot (h-slot-of d-slot))
         (h-slot-name (slot-definition-name h-slot))
         (dimensions (dimensions-of d-slot))
         (query (make-query nil `(d-instance ,(mapcar #'name-of dimensions))))
         result)

    (add-query-variable query 'h-instance)
    (add-collect query `(slot-value h-instance ',h-slot-name))
    (add-assert query `(typep h-instance ,h-class-name))
    (add-assert query `(eq (d-object-of h-instance) d-instance))
    (add-assert query `(or
                        (not (slot-boundp h-instance ',h-slot-name))
                        (not (eq (slot-value h-instance ',h-slot-name) ,+h-unused-slot-marker+))))
    
    (iter (for dimension :in dimensions)
          (for coordinate :in coordinates)
          (for interval-p = (when (typep dimension 'ordering-dimension)
                              (assert (funcall (dimension-less-or-equal dimension)
                                               (car coordinate) (cdr coordinate)))
                              (funcall (dimension-less dimension) (car coordinate) (cdr coordinate))))

          (etypecase dimension

            (inheriting-dimension

             (add-collect query `(cons
                                  (slot-value h-instance ',(slot-name-of dimension))
                                  ,(cdr coordinate))) ;; FIXME should be inclusive if not interval-p

             (add-assert query
                         `(,(if interval-p
                                (dimension-less dimension)
                                (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(name-of dimension))
                            (cdr ,(name-of dimension))))

             (when (and (length= 1 dimensions) interval-p)
               (add-assert query
                           `(,(dimension-less-or-equal dimension)
                              ;; TODO should be a subselect:
                              ;;  (select ((max (slot-value h-instance ',(name-of dimension))))
                              ;;     (from (h-instance ,h-class-name))
                              ;;     (where ...)))
                              (car ,(name-of dimension))
                              (slot-value h-instance ',(name-of dimension)))))

             (if (and (length= 1 dimensions) (not interval-p))
                 (bind ((direction (ecase (direction-of dimension)
                                     (:ascending :descending)
                                     (:descending :ascending))))
                   (setf (limit-of query) 1)
                   (add-order-by query `(slot-value h-instance ',(slot-name-of dimension)) direction)
                   (add-order-by query 'h-instance direction))
                 (progn
                   (add-order-by query `(slot-value h-instance ',(slot-name-of dimension)) (direction-of dimension))
                   (add-order-by query 'h-instance (direction-of dimension)))))

            (ordering-dimension
             (add-collect query `(cons
                                  (slot-value h-instance ',(begin-slot-name-of dimension))
                                  (slot-value h-instance ',(end-slot-name-of dimension))))
             (add-assert query
                         `(,(if interval-p (dimension-less dimension) (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(begin-slot-name-of dimension))
                            (cdr ,(name-of dimension)))) ; end
             (add-assert query
                         `(,(dimension-less dimension)
                            (car ,(name-of dimension)) ; start
                            (slot-value h-instance ',(end-slot-name-of dimension)))))

            (dimension
             (add-collect query `(slot-value h-instance ',(slot-name-of dimension)))
             (add-assert query
                         `(member
                           (slot-value h-instance ',(slot-name-of dimension))
                           ,(name-of dimension))))))

    (unless (order-by-of query)
      ;; order by _id column (as version)
      (add-order-by query 'h-instance :ascending))

    (setf result (apply #'execute-query query d-instance coordinates))

    ;; This can be deleted if the subselect above can be added to the query
    ;; (adds the missing record that is the last before the beginning of the interval)
    (when (and (length= 1 dimensions)
               (typep (first dimensions) 'inheriting-dimension)
               (funcall (dimension-less (first dimensions))
                        (car (first coordinates))
                        (cdr (first coordinates))))
      (bind ((dimension (first dimensions))
             (coordinate (first coordinates))
             (last-coordinate (car (second (lastcar result)))))
        (when (and last-coordinate
                   (not (funcall (dimension-equal dimension)
                                 (car coordinate) last-coordinate)))
          (setf result
                (append
                 ;; this select returns at most 1 record
                 (select-slot-values-with-dimensions d-class d-instance d-slot
                                                     (cons (car coordinate) (car coordinate)))
                 result)))))

    result))