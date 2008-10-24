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
(def method restore-slot ((d-class persistent-class-d)
                           (d-instance persistent-object-d)
                          (d-slot persistent-effective-slot-definition-d)
                          &key coordinates)
  (bind (((d-slot-default-value . has-default-p) (default-value-for-type-of d-slot))
         (default-value (if has-default-p d-slot-default-value +unbound-slot-marker+))
         (dimensions (dimensions-of d-slot))
         (records (select-slot-values-with-dimensions d-class d-instance d-slot coordinates)))
    (make-d-value-from-records records default-value dimensions coordinates)))

(def method store-slot ((d-class persistent-class-d)
                         (d-instance persistent-object-d)
                        (d-slot persistent-effective-slot-definition-d)
                        value)
  ;; store-all-slots may pass simple values here
  #+nil(assert (d-value-p value))
  (unless (d-value-p value)
    (setf value (make-single-d-value
                 (dimensions-of d-slot)
                 (collect-coordinates-from-variables (dimensions-of d-slot))
                 value)))
  
  (iter (for (coordinates v) :in-d-value value)
        (check-slot-value-type d-instance d-slot v))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-slot d-instance d-slot)

  (iter (for (coordinates v) :in-d-value value)
        (store-slot-t* d-class d-instance d-slot v coordinates)))

(def method lock-slot ((d-instance persistent-object) (d-slot persistent-effective-slot-definition-d) &key (wait t))
  (bind ((h-slot (h-slot-of d-slot))
         (table (name-of (table-of h-slot))))
    (sql-lock-table :table table
                    :mode :exclusive
                    :wait wait)))

(def function store-slot-t* (d-class d-instance d-slot value coordinates)
  (bind ((dimensions (dimensions-of d-slot))
         (inheriting-dimension (find-if [typep !1 'inheriting-dimension] dimensions))
         ((d-slot-default-value . has-default-p) (default-value-for-type-of d-slot))
         (update-count))

    ;; do not store the default value of the slot in a transient instance except if temporal.
    ;; restore-slot interprets missing h-instances as the default value
    (when (and (not (persistent-p d-instance))
               has-default-p
               (equal value d-slot-default-value) ; FIXME equal
               (null inheriting-dimension))
      (return-from store-slot-t*))

    ;; first try to update value in the h-instance having the same coordinates
    (setf update-count (update-h-instance-slot-value d-class d-instance d-slot value coordinates))

    ;; if the update is not succeeded then insert value with t and validity except if it
    ;; is the default value of a non-temporal slot
    (when (zerop update-count)
      (insert-h-instance d-class d-instance d-slot value coordinates))))

;;;
;;; d-value builders
;;;
(def function make-d-value-from-records (records default-value dimensions coordinates)
  (bind ((d-value (make-single-d-value dimensions coordinates default-value)))
    (iter (for record :in records)
          (for value = (first record))
          (for coords = (iter (for dimension :in dimensions)
                              (generating coordinate :in (rest record))
                              (collect (etypecase dimension
                                         (inheriting-dimension
                                          (make-coordinate-range
                                           'ii
                                           (next coordinate)
                                           (maximum-coordinate-of dimension)))
                                         (ordering-dimension
                                          (make-coordinate-range
                                           'ie (next coordinate) (next coordinate)))
                                         (dimension
                                          (next coordinate))))))
          (setf (value-at-coordinates d-value coords) value))
    (value-at-coordinates d-value coordinates))) ; TODO refine cut in inheriting dimension

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
  (format-symbol (symbol-package (name-of dimension)) "~A-BEGIN" (name-of dimension)))

(def (function io) end-slot-name-of (dimension)
  (assert (and (typep dimension 'ordering-dimension) (not (typep dimension 'inheriting-dimension))))
  (format-symbol (symbol-package (name-of dimension)) "~A-END" (name-of dimension)))

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
         (query (make-query `(select (
                                      ;;(if (slot-boundp h-instance ',h-slot-name) KLUDGE fix query compiler
                                      ;;    (slot-value h-instance ',h-slot-name)
                                      ;;    ,+unbound-slot-marker+)
                                      (or (and (not (slot-boundp h-instance ',h-slot-name))
                                               ,+unbound-slot-marker+)
                                          (slot-value h-instance ',h-slot-name)))
                               (from (h-instance ,h-class-name))
                               (where (and (eq (d-instance-of h-instance) d-instance)
                                           (or
                                            (not (slot-boundp h-instance ',h-slot-name))
                                            (not (eq (slot-value h-instance ',h-slot-name)
                                                     ,+h-unused-slot-marker+))))))
                            `(d-instance)))
         result)

    (iter (for dimension :in dimensions)
          (for coordinate :in coordinates)
          (for interval-p = (when (typep dimension 'ordering-dimension)
                              (assert (coordinate<= (coordinate-range-begin coordinate)
                                                    (coordinate-range-end coordinate)))
                              (coordinate< (coordinate-range-begin coordinate)
                                           (coordinate-range-end coordinate))))

          (add-lexical-variable query (name-of dimension))

          (etypecase dimension

            (inheriting-dimension

             (add-collect query `(slot-value h-instance ',(slot-name-of dimension)))

             (add-assert query
                         `(,(if interval-p
                                (dimension-less dimension)
                                (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))

             (when (and (length= 1 dimensions) interval-p)
               (add-assert query
                           `(,(dimension-less-or-equal dimension)
                              ;; TODO should be a subselect:
                              ;;  (select ((max (slot-value h-instance ',(name-of dimension))))
                              ;;     (from (h-instance ,h-class-name))
                              ;;     (where ...)))
                              (coordinate-range-begin ,(name-of dimension))
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
             (add-collect query `(slot-value h-instance ',(begin-slot-name-of dimension)))
             (add-collect query `(slot-value h-instance ',(end-slot-name-of dimension)))
             
             (add-assert query
                         `(,(if interval-p (dimension-less dimension) (dimension-less-or-equal dimension))
                            (slot-value h-instance ',(begin-slot-name-of dimension))
                            (coordinate-range-end ,(name-of dimension))))
             (add-assert query
                         `(,(dimension-less dimension)
                            (coordinate-range-begin ,(name-of dimension))
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
               (coordinate< (coordinate-range-begin (first coordinates))
                            (coordinate-range-end (first coordinates))))
      (bind ((coordinate (first coordinates))
             (last-coordinate (second (first result))))
        (when (or (null last-coordinate)
                  (not (coordinate= (coordinate-range-begin coordinate)
                                    last-coordinate)))
          (setf result
                (append
                 ;; this select returns at most 1 record
                 (select-slot-values-with-dimensions d-class d-instance d-slot
                                                     (list (make-empty-coordinate-range
                                                            (coordinate-range-begin coordinate))))
                 result)))))

    result))

(defun update-h-instance-slot-value (d-class d-instance d-slot value coordinates)

  (bind ((h-class (h-class-of d-class))
         (h-slot (h-slot-of d-slot))
         (dimensions (dimensions-of d-slot))
         (query (make-query
                 `(update (h-instance ,(class-name h-class))
                    (set (slot-value h-instance ',(slot-definition-name h-slot)) value)
                    (where (and (eq (d-instance-of h-instance) d-instance))))
                 '(d-instance value))))

    (iter (for dimension :in dimensions)
          (add-lexical-variable query (name-of dimension))
          (etypecase dimension
            (inheriting-dimension
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(slot-name-of dimension))
                                  (coordinate-range-begin ,(name-of dimension)))))
            (ordering-dimension
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(begin-slot-name-of dimension))
                                  (coordinate-range-begin ,(name-of dimension))))
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(end-slot-name-of dimension))
                                  (coordinate-range-end ,(name-of dimension)))))
            (dimension
             (add-assert query `(,(dimension-equal dimension)
                                  (slot-value h-instance ',(slot-name-of dimension))
                                  ,(name-of dimension))))))

    ;; generate condition for version check
    (add-assert query `(eq h-instance (select ((max (oid-of h-instance-2)))
                                        (from (h-instance-2 ,(class-name h-class)))
                                        (where (and (eq (d-instance-of h-instance-2) d-instance))))))

    (prog1-bind count (apply 'execute-query query d-instance value coordinates)
      (assert (<= count 1) nil "Inconsistent database"))))

(def function insert-h-instance (d-class d-instance d-slot value coordinates)
  (bind ((h-class (h-class-of d-class)))
    (flet ((initarg-of (slot-name)
             (aprog1 (first (slot-definition-initargs (find-persistent-slot h-class slot-name)))
               (assert it))))
      (apply 'make-instance
            h-class
            :d-instance d-instance
            (initarg-of (slot-definition-name d-slot)) value
            (iter (for dimension :in (dimensions-of d-slot))
                  (for coordinate :in coordinates)
                  (etypecase dimension
                    (inheriting-dimension
                     (assert (coordinate= (coordinate-range-begin coordinate)
                                          (coordinate-range-end coordinate)))
                     (collect (initarg-of (slot-name-of dimension)))
                     (collect (coordinate-range-begin coordinate)))
                    (ordering-dimension
                     (collect (initarg-of (begin-slot-name-of dimension)))
                     (collect (coordinate-range-begin coordinate))
                     (collect (initarg-of (end-slot-name-of dimension)))
                     (collect (coordinate-range-end coordinate)))
                    (dimension
                     (collect (initarg-of (slot-name-of dimension)))
                     (collect coordinate))))))))