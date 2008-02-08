;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.


;;; TODO: add key frames to h_table of 1-n,m-n associations
;;; TODO: do not insert the default value (nil or unbound) into the h-table


(in-package :cl-perec)

(defmethod restore-slot ((t-class persistent-class-t) (t-instance t-object) (t-slot persistent-effective-slot-definition-t))
  (flet ((no-value-function (&optional validity-start validity-end)
           (bind ((slot-type (canonical-type-of t-slot)))
             (cond
               ((null-subtype-p slot-type) nil)
               ((unbound-subtype-p slot-type) +unbound-slot-marker+)
               (t (error "No history record for ~S~:[~*~; before ~S~]~:[~2*~; with validity between ~S and ~S~]."
                         t-instance (temporal-p t-slot) (when (temporal-p t-slot) *t*)
                         (time-dependent-p t-slot) validity-start validity-end))))))
    (bind ((records (select-slot-values t-class t-instance t-slot)))
      (if (time-dependent-p t-slot)
          (collect-values-having-validity
           records
           (lambda (record) (restore-slot-value nil (h-slot-of t-slot) record 2))
           (lambda (record) (elt record 0))
           (lambda (record) (elt record 1))
           #'no-value-function
           *validity-start* *validity-end*)
          (if (zerop (length records))
              (no-value-function)
              (restore-slot-value nil (h-slot-of t-slot) (elt-0 records) 0))))))

(defmethod store-slot ((t-class persistent-class-t) (t-instance t-object) (t-slot persistent-effective-slot-definition-t) value)
  (if (values-having-validity-p value)
      (iter (for (v start end) :in-values-having-validity value)
            (check-slot-value-type t-instance t-slot v))
      (check-slot-value-type t-instance t-slot value))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-t-record t-instance t-slot)
    
  (if (time-dependent-p t-slot)
      (if (typep value 'values-having-validity)
          (iter (for (v start end) :in-values-having-validity value) ;; TODO probably suboptimal
                (store-slot-t* t-class t-instance t-slot v start end))
          (store-slot-t* t-class t-instance t-slot value *validity-start* *validity-end*))
      (store-slot-t* t-class t-instance t-slot value +beginning-of-time+ +end-of-time+)))

;;; TODO: do not insert the default value (nil or unbound) into the h-table
;;; TODO: check-slot-value-type (unboundness)
(defun store-slot-t* (t-class t-instance t-slot value validity-start validity-end)
  (assert (or (not (time-dependent-p t-slot))
              (and validity-start validity-end)))
  
  (bind ((h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (update-count))

    ;; first try to update the h-record with the same t/validity
    ;; if it is successful we are done
    (setf update-count (update-h-record-value t-class t-instance t-slot value validity-start validity-end))

    (when (zerop update-count)
      ;; ensure invariant: validity ranges are not overlapping for any given t
      (when (time-dependent-p t-slot)
        (bind ((overlapping-instances (select-current-h-instances-with-overlapping-validity
                                       t-class t-instance t-slot validity-start validity-end)))
          (iter (for h-instance in-sequence overlapping-instances)
                (for validity-start2 = (validity-start-of h-instance))
                (for validity-end2 = (validity-end-of h-instance))
                (for value2 = (if (slot-boundp h-instance h-slot-name)
                                  (slot-value h-instance h-slot-name)
                                  +unbound-slot-marker+))
                (for h-class = (class-of h-instance))
                
                ;; TODO optimize the case when value = value2
                (cond
                  ((and (local-time< validity-start2 validity-start)
                        (local-time<= validity-end2 validity-end))
                   ;; update
                   (store-slot h-class h-instance (validity-end-slot-of t-class) validity-start)) ;; FIXME others used
                  ((and (local-time<= validity-start validity-start2)
                        (local-time< validity-end validity-end2))
                   ;; update
                   (store-slot h-class h-instance (validity-start-slot-of t-class) validity-end)) ;; FIXME others used
                  ((and (local-time< validity-start2 validity-start)
                        (local-time< validity-end validity-end2))
                   ;; update + insert
                   (store-slot h-class h-instance (validity-end-slot-of t-class) validity-start) ;; FIXME others used
                   (insert-h-records t-class t-instance t-slot value2 validity-end validity-end2)) ;; FIXME t-value, copy other slots
                  (t
                   ;; delete
                   (purge-instance h-instance)))))) ;; FIXME other used slots

      ;; insert value with t and validity
      ;; the default value of the slot inserted only if temporal-p and has previous overlapping value
      (insert-h-records t-class t-instance t-slot value validity-start validity-end))

    
    (when (typep value 'persistent-object)
      (invalidate-all-cached-slots value)) ;; FIXME why?
    ;; TODO: if t-instance is cached either invalidate it or set the value on it
    value))

;;;;;;;;;;;;;;;;
;;; Associations

(defun select-1-1-association-t-record (t-class t-instance t-slot)
  (declare (ignore t-class))
  (bind ((table (table-of t-slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of t-slot))
         (validity-start-column (validity-start-column-of t-slot))
         (validity-end-column (validity-end-column-of t-slot))
         (parent-id-column (first (parent-oid-columns-of t-slot)))
         (child-oid-columns (child-oid-columns-of t-slot))
         (records (select-records (append (list validity-start-column validity-end-column)
                                          child-oid-columns)
                                  (list table-name)
                                  :where
                                  (sql-and (id-column-matcher-where-clause t-instance parent-id-column)
                                           (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                                   (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                                           (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                                   (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                           (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                                   (sql-identifier :name (rdbms::name-of validity-end-column))))
                                  :order-by
                                  (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending)))))
    (collect-single-slot-values-having-validity-from-records t-instance t-slot records (child-slot-of t-slot) 2)))

(defun select-1-n-association-t-records (t-class t-instance t-slot)
  (declare (ignore t-class))
  (bind ((table (table-of t-slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of t-slot))
         (validity-start-column (validity-start-column-of t-slot))
         (validity-end-column (validity-end-column-of t-slot))
         (action-column (action-column-of t-slot))
         (parent-id-column (first (parent-oid-columns-of t-slot)))
         (child-oid-columns (child-oid-columns-of t-slot)))
    (collect-children-having-validity
     (child-slot-of t-slot)
     (select-records (append child-oid-columns
                             (list validity-start-column validity-end-column action-column))
                     (list table-name)
                     :where
                     (sql-and (id-column-matcher-where-clause t-instance parent-id-column)
                              (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                      (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                      (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                      (sql-identifier :name (rdbms::name-of validity-end-column))))
                     :order-by
                     (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending)))
     *validity-start*
     *validity-end*)))

(defun insert-1-1-association-t-record (parent t-slot child)
  (bind ((h-class (h-class-of t-slot))
         (table (table-of t-slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of t-slot))
         (validity-start-column (validity-start-column-of t-slot))
         (validity-end-column (validity-end-column-of t-slot))
         (parent-oid-columns (parent-oid-columns-of t-slot))
         (child-oid-columns (child-oid-columns-of t-slot))
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

(defun insert-1-n-association-delta-t-records (parent t-slot children action)
  (bind ((h-class (h-class-of t-slot))
         (table (table-of t-slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of t-slot))
         (validity-start-column (validity-start-column-of t-slot))
         (validity-end-column (validity-end-column-of t-slot))
         (action-column (action-column-of t-slot))
         (parent-oid-columns (parent-oid-columns-of t-slot))
         (child-oid-columns (child-oid-columns-of t-slot)))
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
          (store-slot-value nil (action-slot-of t-slot) action rdbms-values index)
          (insert-record table-name
                         (append +oid-column-names+ parent-oid-columns child-oid-columns
                                 (list t-value-column validity-start-column validity-end-column action-column))
                         rdbms-values))))

;;;
;;; RDBMS access
;;;

;; TODO: use lock-slot
(defun lock-t-record (t-instance t-slot)
  (declare (ignore t-instance t-slot))
  #+nil
  (execute (make-instance 'sql-select
                          :columns (list (lock-column-of t-slot))
                          :tables (list (lock-table-of t-slot))
                          :where (id-column-matcher-where-clause t-instance)
                          :for :update)))

;;; TODO ensure-exported
(defun update-h-record-value (t-class t-instance t-slot value &optional validity-start validity-end)
  (bind ((h-class (h-class-of t-class))
         (value-slot (h-slot-of t-slot))
         (value-columns (columns-of value-slot))
         (parent-id-column (parent-id-column-of t-class))
         (rdbms-values (make-array (length value-columns)))
         validity-start-column-name
         validity-end-column-name
         t-value-column-name
         validity-start-literal
         validity-end-literal
         t-literal
         tables
         where-clause
         count)

    

    (pushnew (name-of (table-of value-slot)) tables)
    (pushnew (name-of (table-of (parent-slot-of t-class))) tables)

    (when (subtypep h-class 'time-dependent-object)
      (bind ((validity-start-column (validity-start-column-of t-class))
             (validity-end-column (validity-end-column-of t-class)))
        (pushnew (name-of (table-of (validity-start-slot-of t-class))) tables)
        (pushnew (name-of (table-of (validity-end-slot-of t-class))) tables)
        (setf validity-start-column-name (rdbms::name-of validity-start-column)
              validity-end-column-name (rdbms::name-of validity-end-column)
              validity-start-literal (sql-literal :value validity-start :type (rdbms::type-of validity-start-column))
              validity-end-literal (sql-literal :value validity-end :type (rdbms::type-of validity-end-column)))))
    
    (when (subtypep h-class 'temporal-object)
      (bind ((t-value-column (t-value-column-of t-class)))
        (pushnew (name-of (table-of (t-value-slot-of t-class))) tables)
        (setf t-value-column-name (rdbms::name-of t-value-column)
              t-literal (sql-literal :value *t* :type (rdbms::type-of t-value-column)))))

    (store-slot-value nil value-slot value rdbms-values 0)
    (setf where-clause
          (apply 'sql-and
                 (id-column-matcher-where-clause t-instance (rdbms::name-of parent-id-column))
                 (append
                  (when (and validity-start-column-name validity-end-column-name)
                    (list
                     (sql-= (sql-identifier :name validity-start-column-name) validity-start-literal)
                     (sql-= (sql-identifier :name validity-end-column-name) validity-end-literal)))
                  (when t-value-column-name
                    (list
                     (sql-= (sql-identifier :name t-value-column-name) t-literal))))))

    (setf count
          (if (length=1 tables)
              (update-records (name-of (table-of value-slot))
                              value-columns
                              rdbms-values
                              where-clause)
              (update-records (name-of (table-of value-slot))
                              value-columns
                              rdbms-values
                              (sql-in (sql-identifier :name +oid-id-column-name+)
                                      (sql-subquery
                                        :query
                                        (sql-select
                                          :columns (list +oid-id-column-name+)
                                          :tables (list
                                                   (reduce #L(sql-joined-table
                                                               :kind :inner
                                                               :left !1
                                                               :right !2
                                                               :using +oid-column-names+)
                                                           (mapcar
                                                            #L(sql-identifier :name !1)
                                                            (remove (name-of (table-of value-slot))
                                                                    tables))))
                                          :where where-clause))))))
    (assert (<= count 1) nil "TODO")
    count))

(defun select-current-h-instances-with-overlapping-validity (t-class t-instance t-slot validity-start validity-end)
  "Return h-records (oid, validity-start, validity-end, value) of T-INSTANCE overlapping with [validity-start,validity-end)."
  (assert (subtypep (h-class-of t-class) 'time-dependent-object))
  
  ;; TODO performance: compile only one query using h-class and h-slot as lexical variables
  (bind ((h-class-name (class-name (h-class-of t-class)))
         (h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (h-slot-reader-name (reader-name-of h-slot))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (where (and
                                       (eq (t-object-of h-instance) ,t-instance)
                                       ;;(not (sql-text ,(format-sql-to-string (unused-check-for h-slot))))
                                       (or
                                        (not (slot-boundp h-instance ',h-slot-name))
                                        (not (eq (,h-slot-reader-name h-instance) ,+h-unused-slot-marker+)))
                                       (local-time< (validity-start-of h-instance) ,validity-end)
                                       (local-time< ,validity-start (validity-end-of h-instance))
                                       ,@(when (temporal-p t-slot)
                                               `((local-time= (t-value-of h-instance) *t*)))))))))
    (execute-query query)))

(defun select-slot-values (t-class t-instance t-slot)
  "Returns the values of the slot (with validity if time-dependent) in descending t order (if temporal). When temporal, but not time-dependent then at most recent selected."
  (bind ((value-slot (h-slot-of t-slot))
         (value-columns (columns-of value-slot))
         (parent-id-column (parent-id-column-of t-class))
         validity-start-column
         validity-end-column
         validity-start-literal
         validity-end-literal
         t-value-column
         t-value-literal
         tables
         validity-columns)

    (pushnew (name-of (table-of value-slot)) tables)
    (pushnew (name-of (table-of (parent-slot-of t-class))) tables)

    (when (time-dependent-p t-slot)
      (setf validity-start-column (validity-start-column-of t-class)
            validity-end-column (validity-end-column-of t-class)
            validity-start-literal (sql-literal :value *validity-start*
                                                :type (rdbms::type-of validity-start-column))
            validity-end-literal (sql-literal :value *validity-end*
                                              :type (rdbms::type-of validity-end-column))
            validity-columns (list validity-start-column validity-end-column))
      (pushnew (name-of (table-of (validity-start-slot-of t-class))) tables))

    (when (temporal-p t-slot)
      (setf t-value-column (t-value-column-of t-class))
      (setf t-value-literal (sql-literal :value *t* :type (rdbms::type-of t-value-column)))
      (pushnew (name-of (table-of (t-value-slot-of t-class))) tables))
    
    (select-records
     (append validity-columns value-columns)
     ;;
     (list (reduce #L(sql-joined-table :kind :inner :left !1 :right !2 :using +oid-column-names+)
                   (mapcar #L(sql-identifier :name !1) tables)))
     :where
     (sql-and (id-column-matcher-where-clause t-instance parent-id-column)
              (sql-not (unused-check-for value-slot))
              (apply #'sql-and
                     (append
                      (when (time-dependent-p t-slot)
                        (list
                         (sql-< (sql-identifier :name (rdbms::name-of validity-start-column))
                                validity-end-literal)
                         (sql-< validity-start-literal
                                (sql-identifier :name (rdbms::name-of validity-end-column)))))
                      (when (temporal-p t-slot)
                        (list
                         (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                 t-value-literal))))))
     :order-by
     (when (temporal-p t-slot)
       (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column))
                            :ordering :descending)))
     :limit
     (unless (time-dependent-p t-slot)
       1))))

(defun insert-h-records (t-class t-instance t-slot value validity-start validity-end)
  (bind ((h-class (h-class-of t-class)))
    (apply 'make-instance
           h-class
           :t-object t-instance
           (first (slot-definition-initargs t-slot)) value
           (append
            (when (subtypep h-class 'temporal-object)
              (list :t-value *t*))
            (when (subtypep h-class 'time-dependent-object)
              (list :validity-start validity-start
                    :validity-end validity-end))))))

(defun unused-check-for (h-slot)
  (check-for-rdbms-values
   (lisp-value->rdbms-equality-values (canonical-type-of h-slot) +h-unused-slot-marker+)
   (column-names-of h-slot)
   (column-types-of h-slot)
   nil))

