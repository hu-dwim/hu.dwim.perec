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
           (declare (ignorable validity-start validity-end))
           (bind ((slot-type (canonical-type-of t-slot))
                  ((:values t-slot-default-value has-default-p) (default-value-for-type slot-type)))
             (if has-default-p
                 t-slot-default-value
                 ;; There is no record for temporal slots before the time of creation.
                 ;; Consider the slot unbound even if its type does not allow it.
                 +unbound-slot-marker+
                 #+nil
                 (error "No history record for ~S~:[~*~; before ~S~]~:[~2*~; with validity between ~S and ~S~]."
                        t-instance (temporal-p t-slot) (when (temporal-p t-slot) *t*)
                        (time-dependent-p t-slot) validity-start validity-end)))))
    (bind ((records (select-slot-values-with-validity t-class t-instance t-slot)))
      (if (time-dependent-p t-slot)
          (collect-values-having-validity
           records
           (lambda (record) (elt record 0))
           (lambda (record) (elt record 1))
           (lambda (record) (elt record 2))
           #'no-value-function
           *validity-start* *validity-end*)
          (if (zerop (length records))
              (no-value-function)
              (elt records 0))))))

(defmethod store-slot ((t-class persistent-class-t) (t-instance t-object) (t-slot persistent-effective-slot-definition-t) value)
  (if (values-having-validity-p value)
      (iter (for (start end v) :in-values-having-validity value)
            (check-slot-value-type t-instance t-slot v))
      (check-slot-value-type t-instance t-slot value))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-t-slot t-instance t-slot)
    
  (if (time-dependent-p t-slot)
      (if (typep value 'values-having-validity)
          (iter (for (start end v) :in-values-having-validity value) ;; TODO probably suboptimal
                (store-slot-t* t-class t-instance t-slot v start end))
          (store-slot-t* t-class t-instance t-slot value *validity-start* *validity-end*))
      (store-slot-t* t-class t-instance t-slot value +beginning-of-time+ +end-of-time+)))

(defun store-slot-t* (t-class t-instance t-slot value validity-start validity-end)
  (assert (or (not (time-dependent-p t-slot))
              (and validity-start validity-end)))

  (bind ((h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (t-value (when (boundp '*t*) *t*))
         (t-value-slot (t-value-slot-of t-class))
         ((:values t-slot-default-value has-default-p) (default-value-for-type (slot-definition-type t-slot)))
         (update-count))

    ;; do not store the default value of the slot in a transient instance except if temporal
    ;; restore-slot interprets missing h-instances as the default value
    (when (and (not (persistent-p t-instance))
               has-default-p
               (eq value t-slot-default-value)
               (not (temporal-p t-slot)))
      (return-from store-slot-t*))

    ;; first try to update value in the h-instance having the same t and/or validity
    (setf update-count (update-h-instance-slot-value t-class t-instance t-slot value validity-start validity-end))

    ;; if the update is not succeeded then insert value with t and validity except if it
    ;; is the default value of a non-temporal slot
    (when (zerop update-count)
      (unless (and has-default-p (eq value t-slot-default-value) (not (temporal-p t-slot)))
          (insert-h-instance t-class t-instance t-slot value t-value validity-start validity-end)))
    

    ;; ensure invariant: validity ranges are not overlapping for any given t
    (when (and (persistent-p t-instance) (time-dependent-p t-slot))
      (bind ((overlapping-instances (select-current-h-instances-with-overlapping-validity
                                     t-class t-instance t-slot validity-start validity-end)))
        (flet ((all-slots-unused-p (h-instance &key except)
                 (every #L(bind ((slot-name (slot-definition-name !1)))
                            (or (eq slot-name except)
                                (and (slot-boundp h-instance slot-name)
                                     (eq (slot-value h-instance slot-name) +h-unused-slot-marker+))))
                        (persistent-effective-slot-ts-of t-class))))
          (iter (for h-instance in-sequence overlapping-instances)
                (for validity-start2 = (validity-start-of h-instance))
                (for validity-end2 = (validity-end-of h-instance))
                (for t-value2 = (when t-value-slot (t-value-of h-instance)))
                (for value2 = (if (slot-boundp h-instance h-slot-name)
                                  (slot-value h-instance h-slot-name)
                                  +unbound-slot-marker+))
                (for h-class = (class-of h-instance))

                (unless (and (local-time= validity-start validity-start2)
                             (local-time= validity-end validity-end2)
                             (or (null t-value2) (local-time= t-value t-value2)))
                  ;; TODO optimize the case when value = value2
                  (cond
                    ((and (local-time< validity-start2 validity-start)
                          (local-time<= validity-end2 validity-end))
                     ;; update
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (store-slot h-class h-instance (validity-end-slot-of t-class) validity-start)
                         (progn
                           (store-slot h-class h-instance h-slot +h-unused-slot-marker+)
                           (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-start2 validity-start))))
                    ((and (local-time<= validity-start validity-start2)
                          (local-time< validity-end validity-end2))
                     ;; update
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (store-slot h-class h-instance (validity-start-slot-of t-class) validity-end)
                         (progn
                           (store-slot h-class h-instance h-slot +h-unused-slot-marker+)
                           (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-end validity-end2))))
                    ((and (local-time< validity-start2 validity-start)
                          (local-time< validity-end validity-end2))
                     ;; update + insert
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (store-slot h-class h-instance (validity-end-slot-of t-class) validity-start)
                         (progn
                           (store-slot h-class h-instance h-slot +h-unused-slot-marker+)
                           (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-start2 validity-start)))
                     (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-end validity-end2))
                    (t
                     ;; delete
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (purge-instance h-instance)
                         (store-slot h-class h-instance h-slot +h-unused-slot-marker+)))))))))

    (when (typep value 'persistent-object)
      (invalidate-all-cached-slots value)) ;; FIXME why?
    ;; TODO: if t-instance is cached either invalidate it or set the value on it

    
    value))

;;;;;;;;;;;;;;;;
;;; Associations

(defmethod restore-slot ((class persistent-class) (instance persistent-object) (t-association-end persistent-association-end-effective-slot-definition-t))
  (labels ((no-value-function (&optional validity-start validity-end)
             (bind ((slot-type (canonical-type-of t-association-end)))
               (cond
                 ((null-subtype-p slot-type) nil)
                 ((unbound-subtype-p slot-type) +unbound-slot-marker+)
                 (t (error "No history record for ~S~:[~*~; before ~S~]~:[~2*~; with validity between ~S and ~S~]."
                           instance (temporal-p t-association-end) (when (temporal-p t-association-end) *t*)
                           (time-dependent-p t-association-end) validity-start validity-end)))))
           
           (unchecked-value (t-association-end instance &optional (validity-start *validity-start*)
                                               (validity-end *validity-end*))
             (bind ((records (select-association-end-values-with-validity t-association-end instance
                                                                          validity-start validity-end))
                    (association-kind (association-kind-of (association-of t-association-end)))
                    (time-dependent-p (time-dependent-p t-association-end)))
               (if (zerop (length records))
                   (if time-dependent-p
                       (make-single-values-having-validity (no-value-function validity-start validity-end)
                                                           validity-start validity-end)
                       (no-value-function))
                   (ecase association-kind
                     (:1-1
                      (if time-dependent-p
                          (collect-values-having-validity
                           records
                           (lambda (record) (elt record 0))
                           (lambda (record) (elt record 1))
                           (lambda (record) (elt record 2))
                           #'no-value-function
                           validity-start validity-end)
                          (elt records 0)))
                     ((:1-n :m-n)
                      (if time-dependent-p
                          (collect-children-having-validity
                           records
                           (lambda (record) (elt record 0))
                           (lambda (record) (elt record 1))
                           (lambda (record) (elt record 2))
                           (lambda (record) (elt record 3))
                           validity-start validity-end)
                          (let (set)
                            (iter (for record :in-sequence records)
                                  (for value = (elt record 0))
                                  (for action = (elt record 1))
                                  (ecase action
                                    (#.+t-insert+ (pushnew value set))
                                    (#.+t-delete+ (deletef value set))))
                            set)))))))
           (check-result (instance result)
             (bind ((other-association-end (other-association-end-of t-association-end)))
               (cond
                 ((or (null result) (eq result +unbound-slot-marker+))
                  result)
                 ((values-having-validity-p result)
                  (bind ((values (make-array 0 :adjustable #t :fill-pointer 0))
                         (validity-starts (make-array 0 :adjustable #t :fill-pointer 0))
                         (validity-ends (make-array 0 :adjustable #t :fill-pointer 0)))
                    (iter (for (start end value) :in-values-having-validity result)
                          (for others = (unchecked-value other-association-end value start end))
                          (iter (for (other-start other-end other-value) :in-values-having-validity others)
                                (if (find instance (ensure-list other-value))
                                    (vector-push-extend value values)
                                    (vector-push-extend (no-value-function) values))
                                (vector-push-extend other-start validity-starts)
                                (vector-push-extend other-end validity-ends)))
                    (make-values-having-validity values validity-starts validity-ends)))
                 ((persistent-object-p result)
                  (if (find instance (ensure-list (unchecked-value other-association-end result)))
                      result
                      (no-value-function)))
                 ((listp result)
                  (remove-if-not #L(find instance (ensure-list (unchecked-value other-association-end !1)))
                                 result))
                 (t
                  (error ""))))))
    
    (check-result instance (unchecked-value t-association-end instance))))

(defmethod store-slot ((class persistent-class) (instance persistent-object) (t-slot persistent-association-end-effective-slot-definition-t) value)
(if (values-having-validity-p value)
      (iter (for (start end v) :in-values-having-validity value)
            (check-slot-value-type instance t-slot v))
      (check-slot-value-type instance t-slot value))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-t-slot instance t-slot)
    
  (if (time-dependent-p t-slot)
      (if (typep value 'values-having-validity)
          (iter (for (start end v) :in-values-having-validity value) ;; TODO probably suboptimal
                (store-t-association-end t-slot instance v start end))
          (store-t-association-end t-slot instance value *validity-start* *validity-end*))
      (store-t-association-end t-slot instance value +beginning-of-time+ +end-of-time+)))

(def function store-t-association-end (t-association-end instance value validity-start validity-end)
  (assert (or (not (time-dependent-p t-association-end))
              (and validity-start validity-end)))

  (bind ((temporal-p (temporal-p t-association-end))
         (time-dependent-p (time-dependent-p t-association-end))
         (t-value (when (boundp '*t*) *t*))
         ((:values t-slot-default-value has-default-p) (default-value-for-type (slot-definition-type t-association-end)))
         (default-value-p (and has-default-p (eq value t-slot-default-value)))
         (overlapping-instances (select-current-h-associations-with-overlapping-validity
                                 t-association-end instance value validity-start validity-end)))

    (when (or (not default-value-p) (temporal-p t-association-end))
      (insert-1-1-h-association-end t-association-end instance value
                                    t-value validity-start validity-end))

    (when overlapping-instances
      (bind ((h-class (h-class-of t-association-end))
             (h-slot (h-slot-of t-association-end))
             (h-slot-name (slot-definition-name h-slot))
             (other-h-slot (other-end-h-slot-of t-association-end))
             (other-h-slot-name (slot-definition-name other-h-slot)))
        (iter (for h-instance in-sequence overlapping-instances)
              (for validity-start2 = (when time-dependent-p (validity-start-of h-instance)))
              (for validity-end2 = (when time-dependent-p (validity-end-of h-instance)))
              (for t-value2 = (when temporal-p (t-value-of h-instance)))
              (for instance2 = (if (slot-boundp h-instance h-slot-name)
                                   (slot-value h-instance h-slot-name)
                                   +unbound-slot-marker+))
              (for other-instance2 = (if (slot-boundp h-instance other-h-slot-name)
                                         (slot-value h-instance other-h-slot-name)
                                         +unbound-slot-marker+))
                

              (cond
                ;; |----|      old
                ;;    |----|   new       ->  shrink old
                ((and time-dependent-p
                      (local-time< validity-start2 validity-start)
                      (local-time<= validity-end2 validity-end))
                 (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start))
                ;;   |----|  old
                ;; |----|    new         ->  shrink old
                ((and time-dependent-p
                      (local-time<= validity-start validity-start2)
                      (local-time< validity-end validity-end2))
                 (store-slot h-class h-instance (validity-start-slot-of t-association-end) validity-end))
                ;; |---------|  old
                ;;    |---|     new      -> split old
                ((and time-dependent-p
                      (local-time< validity-start2 validity-start)
                      (local-time< validity-end validity-end2))
                 (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start)
                 (insert-1-1-h-association-end t-association-end instance2 other-instance2
                                               t-value2 validity-end validity-end2))
                ;;    |---|    old
                ;; |---------| new       -> clear instance from old, delete if empty
                (t
                 ;; A - B       new: CB
                 ;;    /        old: AB, CD or CB
                 ;;   /
                 ;; C - D
                 (cond
                   ((and (eq instance instance2) (eq value other-instance2)) ;; CB
                    (purge-instance h-instance))
                   ((eq instance instance2) ;; CD
                    (if other-instance2
                        (store-slot h-class h-instance h-slot nil)
                        (purge-instance h-instance)))
                   ((and value (eq value other-instance2)) ;; AB
                    (if instance2
                        (store-slot h-class h-instance other-h-slot nil)
                        (purge-instance h-instance)))))))))

    value))


;;;
;;; RDBMS access
;;;

;; TODO: use lock-slot
(defun lock-t-slot (t-instance t-slot)
  (declare (ignore t-instance t-slot))
  #+nil
  (execute (make-instance 'sql-select
                          :columns (list (lock-column-of t-slot))
                          :tables (list (lock-table-of t-slot))
                          :where (id-column-matcher-where-clause t-instance)
                          :for :update)))

;;; TODO ensure-exported
(defun update-h-instance-slot-value (t-class t-instance t-slot value &optional validity-start validity-end)
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
    (assert (<= count 1) nil "Inconsistent database")
    count))

(defun select-current-h-instances-with-overlapping-validity (t-class t-instance t-slot validity-start validity-end)
  "Return h-instances of T-INSTANCE having value of T-SLOT and overlapping with [VALIDITY-START,VALIDITY-END)."
  (assert (subtypep (h-class-of t-class) 'time-dependent-object))
  
  ;; TODO performance: compile only one query using h-class and h-slot as lexical variables
  (bind ((h-class-name (class-name (h-class-of t-class)))
         (h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (h-slot-reader-name (reader-name-of h-slot))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (where (and
                                       (eq (t-object-of h-instance) t-instance)
                                       (or
                                        (not (slot-boundp h-instance ',h-slot-name))
                                        (not (eq (,h-slot-reader-name h-instance) ,+h-unused-slot-marker+)))
                                       (local-time< (validity-start-of h-instance) validity-end)
                                       (local-time< validity-start (validity-end-of h-instance))
                                       ,@(when (temporal-p t-slot)
                                               `((local-time= (t-value-of h-instance) t-value))))))
                            '(t-instance validity-start validity-end t-value))))
    (execute-query query t-instance validity-start validity-end (when (temporal-p t-slot) *t*))))

(defun select-slot-values-with-validity (t-class t-instance t-slot)
  "Returns the values of the slot (with validity if time-dependent) in descending t order (if temporal). When temporal, but not time-dependent then only the most recent queried."
  (bind ((h-class-name (class-name (h-class-of t-class)))
         (h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (h-slot-reader-name (reader-name-of h-slot))
         ;; TODO performance: compile only one query using h-class and h-slot as lexical variables
         (query (make-query `(select
                               ((,h-slot-reader-name h-instance)
                                ,@(when (time-dependent-p t-slot)
                                        `((validity-start-of h-instance)
                                          (validity-end-of h-instance))))
                               (from (h-instance ,h-class-name))
                               (where (and
                                       (eq (t-object-of h-instance) t-instance)
                                       (or
                                        (not (slot-boundp h-instance ',h-slot-name))
                                        (not (eq (,h-slot-reader-name h-instance) ,+h-unused-slot-marker+)))
                                       ,@(when (time-dependent-p t-slot)
                                               `((local-time< (validity-start-of h-instance) validity-end)
                                                 (local-time< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-slot)
                                               `((local-time<= (t-value-of h-instance) t-value)))))
                               ,@(when (temporal-p t-slot)
                                       `((order-by :descending (t-value-of h-instance))))
                               ,@(unless (time-dependent-p t-slot)
                                         `((limit 1))))
                            '(t-instance validity-start validity-end t-value))))

    (execute-query query t-instance *validity-start* *validity-end* (when (temporal-p t-slot) *t*))))

(defun insert-h-instance (t-class t-instance t-slot value t-value validity-start validity-end)
  (bind ((h-class (h-class-of t-class)))
    (apply 'make-instance
           h-class
           :t-object t-instance
           (first (slot-definition-initargs t-slot)) value
           (append
            (when (subtypep h-class 'temporal-object)
              (list :t-value t-value))
            (when (subtypep h-class 'time-dependent-object)
              (list :validity-start validity-start
                    :validity-end validity-end))))))

;;
;; Association helpers
;;

(defun select-association-end-values-with-validity (t-association-end instance
                                                    &optional
                                                    (validity-start *validity-start*)
                                                    (validity-end *validity-end*))
  "Returns the values of the association-end (with validity if time-dependent) in descending t order (if temporal). When temporal, but not time-dependent then only the most recent queried."
  (bind ((h-class-name (class-name (h-class-of t-association-end)))
         (h-association-end (h-slot-of t-association-end))
         (h-association-end-reader-name (reader-name-of h-association-end))
         (other-h-association-end (other-end-h-slot-of t-association-end))
         (other-h-association-end-reader-name (reader-name-of other-h-association-end))
         (association-kind (association-kind-of (association-of t-association-end)))
         
         ;; TODO performance: compile only one query
         (query (make-query `(select
                               ((,other-h-association-end-reader-name h-instance)
                                ,@(when (time-dependent-p t-association-end)
                                        `((validity-start-of h-instance)
                                          (validity-end-of h-instance)))
                                ,@(unless (eq association-kind :1-1)
                                        `((action-of h-instance))))
                               (from (h-instance ,h-class-name))
                               (where (and
                                       (eq (,h-association-end-reader-name h-instance) instance)
                                       ,@(when (time-dependent-p t-association-end)
                                               `((local-time< (validity-start-of h-instance) validity-end)
                                                 (local-time< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-association-end)
                                               `((local-time<= (t-value-of h-instance) t-value)))))
                               ,@(when (temporal-p t-association-end)
                                       `((order-by :descending (t-value-of h-instance))))
                               ,@(when (and (not (time-dependent-p t-association-end))
                                            (eq association-kind :1-1))
                                         `((limit 1))))
                            '(instance validity-start validity-end t-value)))
         (result (execute-query query instance validity-start validity-end (when (temporal-p t-association-end) *t*))))

    result

    ))

(defun select-current-h-associations-with-overlapping-validity (t-association-end instance value
                                                                &optional validity-start validity-end)

  (assert (or (not (time-dependent-p t-association-end))
              (and validity-start validity-end)))
  
  ;; TODO performance: compile only one query using lexical variables
  (bind ((h-class-name (class-name (h-class-of t-association-end)))
         (h-slot-reader-name (reader-name-of (h-slot-of t-association-end)))
         (other-h-slot-reader-name (reader-name-of (other-end-h-slot-of t-association-end)))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (where (and
                                       (or
                                        (eq (,h-slot-reader-name h-instance) instance)
                                        (and (not (null value))
                                             (eq (,other-h-slot-reader-name h-instance) value)))
                                       ,@(when (time-dependent-p t-association-end)
                                               `((local-time< (validity-start-of h-instance) validity-end)
                                                 (local-time< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-association-end)
                                               `((local-time= (t-value-of h-instance) t-value))))))
                            '(instance value validity-start validity-end t-value))))
    (execute-query query instance value validity-start validity-end (when (temporal-p t-association-end) *t*))))


(defun update-1-1-association-h-instance (t-association-end instance other-instance &optional validity-start validity-end)
  (bind ((h-class (h-class-of t-association-end))
         (reader (reader-name-of (h-slot-of t-association-end)))
         (other-reader (reader-name-of (other-end-h-slot-of t-association-end)))
         (query (make-query
                 `(update (h-instance ,h-class)
                    (set (,other-reader h-instance) other-instance)
                    (where (and (eq (,reader h-instance) instance)
                                ,@(when (temporal-p t-association-end)
                                        (list `(eq (t-value-of h-instance) t-value)))
                                ,@(when (time-dependent-p t-association-end)
                                        (list `(and (eq (validity-start-of h-instance) validity-start)
                                                    (eq (validity-end-of h-instance) validity-end)))))))
                 '(instance other-instance t-value validity-start validity-end)))
         (count (execute-query query instance other-instance *t* validity-start validity-end)))
    (assert (<= count 1) nil "Inconsistent database")
    count))

(defun insert-1-1-h-association-end (t-association-end instance other-instance t-value validity-start validity-end)
  (bind ((h-class (h-class-of t-association-end))
         (h-init-arg (first (slot-definition-initargs (h-slot-of t-association-end))))
         (other-h-init-arg (first (slot-definition-initargs (other-end-h-slot-of t-association-end)))))
    (apply 'make-instance
           h-class
           (append
            (list h-init-arg instance)
            (list other-h-init-arg other-instance)
            (when (subtypep h-class 'temporal-object)
              (list :t-value t-value))
            (when (subtypep h-class 'time-dependent-object)
              (list :validity-start validity-start
                    :validity-end validity-end))))))

