;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defun restore-slot-t (class instance slot)
  (bind ((time-dependent-p (time-dependent-p slot))
         (temporal-p (temporal-p slot))
         (h-slot (h-slot-of slot))
         (table (table-of h-slot))
         (columns (columns-of h-slot))
         (parent-id-column (parent-id-column-of class))
         (t-value-column (when temporal-p
                           (t-value-column-of class)))
         (validity-start-column (when time-dependent-p
                                  (validity-start-column-of class)))
         (validity-end-column (when time-dependent-p
                                (validity-end-column-of class)))
         (records
          ;; TODO: add row limit = 1 if not time-dependent
          (select-records (append
                           (when time-dependent-p
                             (list validity-start-column validity-end-column))
                           columns)
                          (list (name-of table))
                          (sql-and (id-column-matcher-where-clause instance parent-id-column)
                                   ;; TODO: hack this out with reader
                                   (sql-is-not-null (sql-identifier :name (rdbms::name-of (first columns))))
                                   (apply #'sql-and
                                          (append
                                           (when time-dependent-p
                                             (list
                                              (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                                      (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                              (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                                      (sql-identifier :name (rdbms::name-of validity-end-column)))))
                                           (when temporal-p
                                             (list
                                              (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                                      (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f))))))))
                          (when temporal-p
                            (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending))))))
    (if time-dependent-p
        (bind ((values-having-validity
                (setf (underlying-slot-value-using-class class instance slot)
                      (collect-single-slot-values-having-validity-from-records instance slot records h-slot 2)))
               (values (values-of values-having-validity)))
          (if (= 1 (length values))
              (elt values 0)
              values-having-validity))
        (if (zerop (length records))
            (slot-unbound-t instance slot)
            (setf (underlying-slot-value-using-class class instance slot)
                  (restore-slot-value h-slot (elt-0 records) 0))))))

(defun store-slot-t (class instance slot value)
  (bind ((time-dependent-p (time-dependent-p slot))
         (temporal-p (temporal-p slot))
         (h-class (h-class-of class))
         (h-slot (h-slot-of slot))
         (table (table-of h-slot))
         (table-name (name-of table))
         (columns (columns-of h-slot))
         (column-count (length columns))
         (parent-id-column (parent-id-column-of class))
         (t-value-column (when temporal-p
                           (t-value-column-of class)))
         (validity-start-column (when time-dependent-p
                                  (validity-start-column-of class)))
         (validity-end-column (when time-dependent-p
                                (validity-end-column-of class)))
         (rdbms-values (make-array column-count))
         (update-count))
    (store-slot-value h-slot value rdbms-values 0)
    (setf update-count
          (update-records table-name
                          columns
                          rdbms-values
                          (apply 'sql-and (id-column-matcher-where-clause instance parent-id-column)
                                 (append
                                  (when time-dependent-p
                                    (list
                                     (sql-= (sql-identifier :name (rdbms::name-of validity-start-column))
                                            (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
                                     (sql-= (sql-identifier :name (rdbms::name-of validity-end-column))
                                            (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))))
                                  (when temporal-p
                                    (list
                                     (sql-= (sql-identifier :name (rdbms::name-of t-value-column))
                                            (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))))))))
    (assert (<= update-count 1))
    (when (zerop update-count)
      (unless temporal-p
        (bind ((records
                (select-records (list validity-start-column validity-end-column)
                                (list (name-of table))
                                (sql-and (id-column-matcher-where-clause instance parent-id-column)
                                         ;; TODO: hack this out with reader
                                         (sql-is-not-null (sql-identifier :name (rdbms::name-of (first columns))))
                                         (sql-and (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                                          (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                                  (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                                          (sql-identifier :name (rdbms::name-of validity-end-column))))))))
          (assert (zerop (length records)) nil "Setting a time dependent but not temporal slot for overlapping validity ranges is not yet supported: between ~A ~A and ~A ~A"
                  *validity-start* *validity-end* (elt-0 (elt-0 records)) (elt-1 (elt-0 records)))))
      (bind ((size (+ (* 2 +oid-column-count+) 3 column-count))
             (rdbms-values (make-array size))
             (index 0))
        (oid->rdbms-values* (make-class-oid (class-name h-class)) rdbms-values index)
        (incf index +oid-column-count+)
        (store-slot-value (parent-slot-of class) instance rdbms-values index)
        (incf index +oid-column-count+)
        (when temporal-p
          (setf (aref rdbms-values index) (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
          (incf index))
        (when time-dependent-p
          (setf (aref rdbms-values index) (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
          (incf index)
          (setf (aref rdbms-values index) (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
          (incf index))
        (store-slot-value h-slot value rdbms-values index)
        (insert-record table-name
                       (append +oid-column-names+
                               (list parent-id-column)
                               (when temporal-p
                                 (list t-value-column))
                               (when time-dependent-p
                                 (list validity-start-column validity-end-column))
                               columns)
                       rdbms-values)))
    (when (typep value 'persistent-object)
      (invalidate-all-cached-slots value))
    ;; TODO: if t-instance is cached either invalidate it or set the value on it
    value))

;;;;;;;;;;;;;;;;
;;; Associations

(defun select-1-1-association-t-record (instance slot)
  (bind ((table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (parent-id-column (first (parent-oid-columns-of slot)))
         (child-oid-columns (child-oid-columns-of slot))
         (records
          (select-records (append (list validity-start-column validity-end-column)
                                  child-oid-columns)
                          (list table-name)
                          (sql-and (id-column-matcher-where-clause instance parent-id-column)
                                   (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                           (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                                   (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                           (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                   (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                           (sql-identifier :name (rdbms::name-of validity-end-column))))
                          (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending)))))
    (collect-single-slot-values-having-validity-from-records instance slot records (child-slot-of slot) 2)))

(defun select-1-n-association-t-records (instance slot)
  (bind ((table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (action-column (action-column-of slot))
         (parent-id-column (first (parent-oid-columns-of slot)))
         (child-oid-columns (child-oid-columns-of slot)))
    (collect-children-having-validity
     (child-slot-of slot)
     (select-records (append child-oid-columns
                             (list validity-start-column validity-end-column action-column))
                     (list table-name)
                     (sql-and (id-column-matcher-where-clause instance parent-id-column)
                              (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                      (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                      (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                      (sql-identifier :name (rdbms::name-of validity-end-column))))
                     (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending)))
     *validity-start*
     *validity-end*)))

(defun insert-1-1-association-t-record (parent slot child)
  (bind ((h-class (h-class-of slot))
         (table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (parent-oid-columns (parent-oid-columns-of slot))
         (child-oid-columns (child-oid-columns-of slot))
         (rdbms-values (make-array (+ (* 3 +oid-column-count+) 4)))
         (index 0))
    (oid->rdbms-values* (make-class-oid (class-name h-class)) rdbms-values index)
    (incf index +oid-column-count+)
    (oid->rdbms-values* (oid-of parent) rdbms-values index)
    (incf index +oid-column-count+)
    (oid->rdbms-values* (oid-of child) rdbms-values index)
    (incf index +oid-column-count+)
    (setf (aref rdbms-values index) (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
    (incf index)
    (setf (aref rdbms-values index) (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
    (incf index)
    (setf (aref rdbms-values index) (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
    (insert-record table-name
                   (append +oid-column-names+ parent-oid-columns child-oid-columns
                           (list t-value-column validity-start-column validity-end-column))
                   rdbms-values)))

(defun insert-1-n-association-delta-t-records (parent slot children action)
  (bind ((h-class (h-class-of slot))
         (table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (action-column (action-column-of slot))
         (parent-oid-columns (parent-oid-columns-of slot))
         (child-oid-columns (child-oid-columns-of slot)))
    (iter (for child :in children)
          (for rdbms-values = (make-array (+ (* 3 +oid-column-count+) 4)))
          (for index = 0)
          (oid->rdbms-values* (make-class-oid (class-name h-class)) rdbms-values index)
          (incf index +oid-column-count+)
          (oid->rdbms-values* (oid-of parent) rdbms-values index)
          (incf index +oid-column-count+)
          (oid->rdbms-values* (oid-of child) rdbms-values index)
          (incf index +oid-column-count+)
          (setf (aref rdbms-values index) (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
          (incf index)
          (setf (aref rdbms-values index) (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
          (incf index)
          (setf (aref rdbms-values index) (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
          (incf index)
          (store-slot-value (action-slot-of slot) action rdbms-values index)
          (insert-record table-name
                         (append +oid-column-names+ parent-oid-columns child-oid-columns
                                 (list t-value-column validity-start-column validity-end-column action-column))
                         rdbms-values))))
