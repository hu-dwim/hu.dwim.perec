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
           (bind (((t-slot-default-value . has-default-p) (default-value-for-type-of t-slot)))
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
  (lock-slot t-instance t-slot)

  (if (time-dependent-p t-slot)
      (if (typep value 'values-having-validity)
          (iter (for (start end v) :in-values-having-validity value) ;; TODO probably suboptimal
                (store-slot-t* t-class t-instance t-slot v start end))
          (store-slot-t* t-class t-instance t-slot value *validity-start* *validity-end*))
      (store-slot-t* t-class t-instance t-slot value +beginning-of-time+ +end-of-time+)))

(defmethod lock-slot ((t-instance persistent-object) (t-slot persistent-effective-slot-definition-t) &key (wait t))
  (bind ((h-slot (h-slot-of t-slot))
         (table (name-of (table-of h-slot))))
    (sql-lock-table :table table
                    :mode :exclusive
                    :wait wait)))


(defun store-slot-t* (t-class t-instance t-slot value validity-start validity-end)
  (assert (or (not (time-dependent-p t-slot))
              (and validity-start validity-end)))

  (bind ((h-slot (h-slot-of t-slot))
         (h-slot-name (slot-definition-name h-slot))
         (t-value-slot (t-value-slot-of t-class))
         ((t-slot-default-value . has-default-p) (default-value-for-type-of t-slot))
         (update-count))

    ;; do not store the default value of the slot in a transient instance except if temporal.
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
          (insert-h-instance t-class t-instance t-slot value *t* validity-start validity-end)))

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
          (iter (with t-value = *t*)
                (for h-instance in-sequence overlapping-instances)
                (for validity-start2 = (validity-start-of h-instance))
                (for validity-end2 = (validity-end-of h-instance))
                (for t-value2 = (when t-value-slot (t-value-of h-instance)))
                (for value2 = (if (slot-boundp h-instance h-slot-name)
                                  (slot-value h-instance h-slot-name)
                                  +unbound-slot-marker+))
                (for h-class = (class-of h-instance))

                (unless (and (timestamp= validity-start validity-start2)
                             (timestamp= validity-end validity-end2)
                             (or (null t-value2)
                                 (timestamp= t-value t-value2)))
                  ;; TODO optimize the case when value = value2
                  (cond
                    ((and (timestamp< validity-start2 validity-start)
                          (timestamp<= validity-end2 validity-end))
                     ;; update
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (store-slot h-class h-instance (validity-end-slot-of t-class) validity-start)
                         (progn
                           (store-slot h-class h-instance h-slot +h-unused-slot-marker+)
                           (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-start2 validity-start))))
                    ((and (timestamp<= validity-start validity-start2)
                          (timestamp< validity-end validity-end2))
                     ;; update
                     (if (all-slots-unused-p h-instance :except h-slot-name)
                         (store-slot h-class h-instance (validity-start-slot-of t-class) validity-end)
                         (progn
                           (store-slot h-class h-instance h-slot +h-unused-slot-marker+)
                           (insert-h-instance t-class t-instance t-slot value2 t-value2 validity-end validity-end2))))
                    ((and (timestamp< validity-start2 validity-start)
                          (timestamp< validity-end validity-end2))
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
      (invalidate-cached-instance value)) ;; FIXME why?
    ;; TODO: if t-instance is cached either invalidate it or set the value on it


    value))

;;;;;;;;;;;;;;;;
;;; Associations
(defmethod restore-slot ((t-class persistent-class-t) (t-instance t-object) (t-association-end persistent-association-end-effective-slot-definition-t))
  (restore-t-association-end t-instance t-association-end))

(defmethod restore-slot ((class persistent-class) (instance persistent-object) (t-association-end persistent-association-end-effective-slot-definition-t))
  (restore-t-association-end instance t-association-end))

(defun restore-t-association-end (instance t-association-end)
  (labels ((no-value-function (&optional validity-start validity-end)
             (bind ((slot-type (canonical-type-of t-association-end)))
               (cond
                 ((null-subtype-p slot-type) nil)
                 ((unbound-subtype-p slot-type) +unbound-slot-marker+)
                 (t (error "No history record for ~S~:[~*~; before ~S~]~:[~2*~; with validity between ~S and ~S~]."
                           instance (temporal-p t-association-end) (when (temporal-p t-association-end) *t*)
                           (time-dependent-p t-association-end) validity-start validity-end)))))

           (collect-1-1-values (records validity-start validity-end)
             (collect-values-having-validity
              records
              (lambda (record) (elt record 0))
              (lambda (record) (elt record 1))
              (lambda (record) (elt record 2))
              #'no-value-function ;; FIXME depends on association-end!!
              validity-start validity-end))

           (collect-value (records cardinality)
             (let (set)
               (iter (for record :in-sequence records :from (1- (length records)) :downto 0)
                     (for value = (elt record 0))
                     (for action = (elt record 1))
                     (ecase action
                       (#.+t-clear+ (setf set nil))
                       (#.+t-insert+ (pushnew value set))
                       (#.+t-delete+ (deletef set value))))
               (ecase cardinality
                 (:1 (if set (last-elt set) nil))
                 (:n set))))

           (collect-time-dependent-value (records cardinality validity-start validity-end)
             (bind  ((result (collect-children-having-validity
                              records
                              (lambda (record) (elt record 0))
                              (lambda (record) (elt record 1))
                              (lambda (record) (elt record 2))
                              (lambda (record) (elt record 3))
                              validity-start validity-end)))
               (ecase cardinality
                 (:1 (iter (for (s e v) :in-values-having-validity result)
                           (collect-value-with-validity (if v (last-elt v) v) :from s :to e)))
                 (:n result))))

           (unchecked-value (t-association-end instance &optional (validity-start *validity-start*)
                                               (validity-end *validity-end*))
             (bind ((records (select-association-end-values-with-validity t-association-end instance
                                                                          validity-start validity-end))
                    (association-kind (association-kind-of (association-of t-association-end)))
                    (time-dependent-p (time-dependent-p t-association-end)))
               #+nil(format t "assoc-end: ~S instance: ~S start: ~S end: ~S records: ~S~%"
                            t-association-end instance validity-start validity-end records)
               (if (zerop (length records))
                   (if time-dependent-p
                       (make-single-values-having-validity (no-value-function validity-start validity-end)
                                                           validity-start validity-end)
                       (no-value-function))
                   (ecase association-kind
                     (:1-1
                      (if time-dependent-p
                          (collect-1-1-values records validity-start validity-end)
                          (elt records 0)))
                     (:1-n
                      (if time-dependent-p
                          (collect-time-dependent-value records (cardinality-kind-of t-association-end)
                                                        validity-start validity-end)
                          (collect-value records (cardinality-kind-of t-association-end))))
                     (:m-n
                      (if time-dependent-p
                          (collect-time-dependent-value records :n validity-start validity-end)
                          (collect-value records :n)))))))

           (check-result (instance result)
             (bind ((other-association-end (other-association-end-of t-association-end)))
               (cond
                 ((or (null result) (eq result +unbound-slot-marker+))
                  result)
                 ((values-having-validity-p result)
                  (bind ((values (make-array 0 :adjustable #t :fill-pointer 0))
                         (validity-starts (make-array 0 :adjustable #t :fill-pointer 0))
                         (validity-ends (make-array 0 :adjustable #t :fill-pointer 0)))
                    (flet ((add-with-validity (value validity-start validity-end)
                             (vector-push-extend value values)
                             (vector-push-extend validity-start validity-starts)
                             (vector-push-extend validity-end validity-ends)))
                      (iter (for (start end value) :in-values-having-validity result)
                            ;;(format t "Check ~S~%" value)
                            (cond
                              ((or (null value) (eq value +unbound-slot-marker+))
                               (add-with-validity value start end))
                              ((persistent-object-p value)
                               (iter (for (other-start other-end other-value) :in-values-having-validity
                                          (unchecked-value other-association-end value start end))
                                     (if (find instance (ensure-list other-value))
                                         (add-with-validity value other-start other-end)
                                         (add-with-validity (no-value-function) other-start other-end))))
                              ((listp value)
                               (bind ((gen (apply 'values-having-validity-generator
                                                  (mapcar
                                                   #L(unchecked-value other-association-end !1 start end)
                                                   value))))
                                 (iter (for (values s e vs) = (funcall gen))
                                       (while s)
                                       (add-with-validity
                                        (iter (for vv :in value)
                                              (for v :in-sequence vs)
                                              (when (and (not (eq v +missing-value+))
                                                         (find instance (ensure-list v)))
                                                (adjoining vv)))
                                        s e))))
                              (t
                               (error "Bug")))))
                    (make-values-having-validity values validity-starts validity-ends)))
                 ((persistent-object-p result)
                  (if (find instance (ensure-list (unchecked-value other-association-end result)))
                      result
                      (no-value-function)))
                 ((listp result)
                  (remove-if-not #L(find instance (ensure-list (unchecked-value other-association-end !1)))
                                 result))
                 (t
                  (error "Bug"))))))

    (if (and *lazy-slot-value-collections*
             (eq (cardinality-kind-of t-association-end) :n))
        (make-instance 'persistent-association-end-set-container-t
                       :instance instance :slot t-association-end)
        (check-result instance (unchecked-value t-association-end instance)))))

(defmethod store-slot ((t-class persistent-class-t) (t-instance t-object) (t-slot persistent-association-end-effective-slot-definition-t) value)
  (store-t-association-end t-instance t-slot value))

(defmethod store-slot ((class persistent-class) (instance persistent-object) (t-slot persistent-association-end-effective-slot-definition-t) value)
  (store-t-association-end instance t-slot value))

(def function store-t-association-end (instance t-association-end value)
  ;; FIXME nil is not accepted as (set ...) type ?
  #+nil
  (if (values-having-validity-p value)
      (iter (for (start end v) :in-values-having-validity value)
            (check-slot-value-type instance t-association-end v))
      (check-slot-value-type instance t-association-end value))

  ;; this lock ensures that
  ;; the insert/update operations on the h-table are serialized properly.
  (lock-slot instance t-association-end)

  (flet ((store-one-value (value validity-start validity-end)
           (ecase (association-kind-of (association-of t-association-end))
             (:1-1 (store-1-1-t-association-end t-association-end instance value validity-start validity-end))
             (:1-n (store-1-n-t-association-end t-association-end instance value validity-start validity-end))
             (:m-n (store-m-n-t-association-end t-association-end instance value validity-start validity-end)))))
    (if (time-dependent-p t-association-end)
        (if (typep value 'values-having-validity)
            (iter (for (start end v) :in-values-having-validity value) ;; TODO probably suboptimal
                  (store-one-value v start end))
            (store-one-value value *validity-start* *validity-end*))
        (store-one-value value +beginning-of-time+ +end-of-time+))))

(defmethod lock-slot ((instance persistent-object) (slot persistent-association-end-effective-slot-definition-t) &key (wait t))
  (bind ((h-class (h-class-of slot))
         (table (name-of (primary-table-of h-class))))
    (execute
     (sql-lock-table :table table
                     :mode :exclusive
                     :wait wait))))


(def function store-1-1-t-association-end (t-association-end instance value validity-start validity-end)
  (assert (or (not (time-dependent-p t-association-end))
              (and validity-start validity-end)))

  (bind ((temporal-p (temporal-p t-association-end))
         (time-dependent-p (time-dependent-p t-association-end))
         (t-value (when temporal-p *t*))
         ((t-slot-default-value . has-default-p) (default-value-for-type-of t-association-end))
         (default-value-p (and has-default-p (eq value t-slot-default-value)))
         (overlapping-instances (select-overlapping-h-associations
                                 t-association-end instance value validity-start validity-end
                                 :criteria :either)))

    (when (or (not default-value-p) (temporal-p t-association-end))
      (insert-h-association-instance t-association-end instance value
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

              ;; insert record that clear the other ends too
              ;; see test/tesites/association/1-1/integrity
              (when (and temporal-p time-dependent-p)
                (bind ((start (timestamp-maximum validity-start validity-start2))
                       (end (timestamp-minimum validity-end validity-end2)))
                  (assert (timestamp< start end))
                  (cond
                    ((and (p-eq instance instance2) other-instance2)

                     (unless (p-eq value other-instance2)
                       (insert-h-association-instance (other-association-end-of t-association-end)
                                                      other-instance2 nil
                                                      t-value start end)))
                    ((and (p-eq value other-instance2) instance2)
                     (unless (p-eq instance instance2)
                       (insert-h-association-instance t-association-end
                                                      instance2 nil
                                                      t-value start end))))))

              (cond
                ;; |----|      old
                ;;    |----|   new       ->  shrink old
                ((and time-dependent-p
                      (timestamp< validity-start2 validity-start)
                      (timestamp<= validity-end2 validity-end))
                 (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start))
                ;;   |----|  old
                ;; |----|    new         ->  shrink old
                ((and time-dependent-p
                      (timestamp<= validity-start validity-start2)
                      (timestamp< validity-end validity-end2))
                 (store-slot h-class h-instance (validity-start-slot-of t-association-end) validity-end))
                ;; |---------|  old
                ;;    |---|     new      -> split old
                ((and time-dependent-p
                      (timestamp< validity-start2 validity-start)
                      (timestamp< validity-end validity-end2))
                 (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start)
                 (insert-h-association-instance t-association-end instance2 other-instance2
                                                t-value2 validity-end validity-end2))
                ;;    |---|    old
                ;; |---------| new       -> clear instance from old, delete if empty
                (t
                 ;; A - B       new: CB
                 ;;    /        old: AB, CD or CB
                 ;;   /
                 ;; C - D
                 ;; FIXME why p-eq needed? eq does not seem to work somehow
                 (cond
                   ((and (p-eq instance instance2) (p-eq value other-instance2)) ;; CB
                    (purge-instance h-instance))
                   ((p-eq instance instance2) ;; CD
                    (if other-instance2
                        (store-slot h-class h-instance h-slot nil)
                        (purge-instance h-instance)))
                   ((and value (p-eq value other-instance2)) ;; AB
                    (if instance2
                        (store-slot h-class h-instance other-h-slot nil)
                        (purge-instance h-instance)))
                   (t
                    (error "Bug")))))

              )))

    value))

(def function store-1-n-t-association-end (t-association-end instance value validity-start validity-end)

  (ecase (cardinality-kind-of t-association-end)
    (:1 ;; set parent
     (bind ((t-value (when (temporal-p t-association-end) *t*))
            (overlapping-instances (select-overlapping-h-associations
                                    t-association-end instance nil validity-start validity-end
                                    :criteria :instance)))
       (insert-h-association-instance t-association-end instance nil
                                      t-value validity-start validity-end
                                      :action +t-clear+)
       (when value
         (insert-h-association-instance t-association-end instance value
                                        t-value validity-start validity-end
                                        :action +t-insert+))

       (consolidate-overlapping-instances t-association-end
                                          overlapping-instances
                                          validity-start validity-end)))
    (:n ;; set children
     (bind ((t-value (when (temporal-p t-association-end) *t*)))

       (bind ((overlapping-instances (select-overlapping-h-associations
                                      t-association-end instance nil validity-start validity-end
                                      :criteria :instance)))
         (insert-h-association-instance t-association-end instance nil
                                        t-value validity-start validity-end
                                        :action +t-clear+)
         (consolidate-overlapping-instances t-association-end
                                            overlapping-instances
                                            validity-start validity-end))

       (dolist (other-instance value)
         (bind ((overlapping-instances (select-overlapping-h-associations
                                        (other-association-end-of t-association-end) other-instance nil
                                        validity-start validity-end
                                        :criteria :instance)))
           (insert-h-association-instance (other-association-end-of t-association-end) other-instance nil
                                          t-value validity-start validity-end
                                          :action +t-clear+)
           (insert-h-association-instance t-association-end instance other-instance
                                          t-value validity-start validity-end
                                          :action +t-insert+)

           (consolidate-overlapping-instances (other-association-end-of t-association-end)
                                              overlapping-instances
                                              validity-start validity-end)))))))

(def function consolidate-overlapping-instances (t-association-end overlapping-instances validity-start validity-end)
  (when overlapping-instances
    (bind ((h-class (h-class-of t-association-end))
           (temporal-p (temporal-p t-association-end))
           (time-dependent-p (time-dependent-p t-association-end))
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
            (for action2 = (action-of h-instance))

            (cond
              ;; |----|      old
              ;;    |----|   new       ->  shrink old
              ((and time-dependent-p
                    (timestamp< validity-start2 validity-start)
                    (timestamp<= validity-end2 validity-end))
               (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start))
              ;;   |----|  old
              ;; |----|    new         ->  shrink old
              ((and time-dependent-p
                    (timestamp<= validity-start validity-start2)
                    (timestamp< validity-end validity-end2))
               (store-slot h-class h-instance (validity-start-slot-of t-association-end) validity-end))
              ;; |---------|  old
              ;;    |---|     new      -> split old
              ((and time-dependent-p
                    (timestamp< validity-start2 validity-start)
                    (timestamp< validity-end validity-end2))
               (store-slot h-class h-instance (validity-end-slot-of t-association-end) validity-start)
               (insert-h-association-instance t-association-end instance2 other-instance2
                                              t-value2 validity-end validity-end2 :action action2))
              ;;    |---|    old
              ;; |---------| new       -> delete old
              (t
               (purge-instance h-instance)))))))

(def function store-m-n-t-association-end (t-association-end instance value validity-start validity-end)
  (bind ((t-value (when (temporal-p t-association-end) *t*))
         (overlapping-instances (select-overlapping-h-associations
                                 t-association-end instance nil validity-start validity-end
                                 :criteria :instance)))

    (insert-h-association-instance t-association-end instance nil
                                   t-value validity-start validity-end
                                   :action +t-clear+)
    (dolist (other-instance value)
      (insert-h-association-instance t-association-end instance other-instance
                                   t-value validity-start validity-end
                                   :action +t-insert+))

    (consolidate-overlapping-instances t-association-end
                                       overlapping-instances
                                       validity-start validity-end)

    value))

(defmethod insert-into-association-end-set-t ((instance persistent-object)
                                              (t-association-end persistent-association-end-effective-slot-definition-t)
                                              (item persistent-object))
  (insert/delete-association-end-set-t t-association-end instance item +t-insert+ *validity-start* *validity-end*))



(defmethod delete-from-association-end-set-t ((instance persistent-object)
                                              (t-association-end persistent-association-end-effective-slot-definition-t)
                                              (item persistent-object))
  (insert/delete-association-end-set-t t-association-end instance item +t-delete+ *validity-start* *validity-end*))

(def function insert/delete-association-end-set-t (t-association-end instance item action validity-start validity-end)

  (lock-slot instance t-association-end)

  (bind ((t-value (when (temporal-p t-association-end) *t*)))
    (when (and (eq (association-kind-of (association-of t-association-end)) :1-n)
               (eq action +t-insert+))
      (bind ((overlapping-instances (select-overlapping-h-associations
                                     (other-association-end-of t-association-end)
                                     item nil validity-start validity-end
                                     :criteria :instance)))
        (insert-h-association-instance (other-association-end-of t-association-end)
                                       item nil
                                       t-value validity-start validity-end
                                       :action +t-clear+)
        (consolidate-overlapping-instances (other-association-end-of t-association-end)
                                           overlapping-instances
                                           validity-start validity-end)))

    (bind ((overlapping-instances (select-overlapping-h-associations
                                   t-association-end instance item validity-start validity-end
                                   :criteria :both)))

      (insert-h-association-instance t-association-end instance item
                                     t-value validity-start validity-end
                                     :action action)

      (consolidate-overlapping-instances t-association-end
                                         overlapping-instances
                                         validity-start validity-end)

      (values))))



;;;
;;; RDBMS access
;;;

;;; TODO ensure-exported
(defun update-h-instance-slot-value (t-class t-instance t-slot value &optional validity-start validity-end)

  (bind ((h-class (h-class-of t-class))
         (reader (reader-name-of (h-slot-of t-slot)))
         (query (make-query
                 `(update (h-instance ,h-class)
                    (set (,reader h-instance) value)
                    (where (and (eq (t-object-of h-instance) t-instance)
                                ,@(when (subtypep h-class 'temporal-object)
                                        (list `(eq (t-value-of h-instance) t-value)))
                                ,@(when (subtypep h-class 'time-dependent-object)
                                        (list `(and (eq (validity-start-of h-instance) validity-start)
                                                    (eq (validity-end-of h-instance) validity-end)))))))
                 '(t-instance value t-value validity-start validity-end)))
         (count (execute-query query t-instance value *t* validity-start validity-end)))
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
                                       (timestamp< (validity-start-of h-instance) validity-end)
                                       (timestamp< validity-start (validity-end-of h-instance))
                                       ,@(when (temporal-p t-slot)
                                               `((timestamp= (t-value-of h-instance) t-value))))))
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
                                               `((timestamp< (validity-start-of h-instance) validity-end)
                                                 (timestamp< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-slot)
                                               `((timestamp<= (t-value-of h-instance) t-value)))))
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
  (check-type instance persistent-object)

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
                                               `((timestamp< (validity-start-of h-instance) validity-end)
                                                 (timestamp< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-association-end)
                                               `((timestamp<= (t-value-of h-instance) t-value)))))
                               (order-by ,@(when (temporal-p t-association-end)
                                                 `(:descending (t-value-of h-instance)))
                                         ,@(unless (eq association-kind :1-1)
                                                   `(:descending (action-of h-instance))))
                               ,@(when (and (not (time-dependent-p t-association-end))
                                            (eq association-kind :1-1))
                                         `((limit 1))))
                            '(instance validity-start validity-end t-value)))
         (result (execute-query query instance validity-start validity-end (when (temporal-p t-association-end) *t*))))

    result))

(defun select-overlapping-h-associations (t-association-end instance value
                                          validity-start validity-end
                                          &key criteria)
  (declare (type persistent-object instance)
           (type (or null persistent-object) value)
           (type (member :instance :both :either) criteria))

  (assert (implies (time-dependent-p t-association-end) (and validity-start validity-end)))

  ;; TODO performance: compile only one query using lexical variables
  (bind ((h-class-name (class-name (h-class-of t-association-end)))
         (h-slot-reader-name (reader-name-of (h-slot-of t-association-end)))
         (other-h-slot-reader-name (reader-name-of (other-end-h-slot-of t-association-end)))
         (instance-criteria (ecase criteria
                              (:instance
                               `(eq (,h-slot-reader-name h-instance) instance))
                              (:both
                               `(and
                                 (eq (,h-slot-reader-name h-instance) instance)
                                 (eq (,other-h-slot-reader-name h-instance) value)))
                              (:either
                               `(or
                                 (eq (,h-slot-reader-name h-instance) instance)
                                 (and (not (null value)) (eq (,other-h-slot-reader-name h-instance) value))))))
         (query (make-query `(select (h-instance)
                               (from (h-instance ,h-class-name))
                               (where (and
                                       ,instance-criteria
                                       ,@(when (time-dependent-p t-association-end)
                                               `((timestamp< (validity-start-of h-instance) validity-end)
                                                 (timestamp< validity-start (validity-end-of h-instance))))
                                       ,@(when (temporal-p t-association-end)
                                               `((timestamp= (t-value-of h-instance) t-value))))))
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

(defun insert-h-association-instance (t-association-end instance other-instance t-value validity-start validity-end &key action)
  (assert (or (null (action-slot-of t-association-end))
              (integerp action)))

  (bind ((h-class (h-class-of t-association-end))
         (h-init-arg (first (slot-definition-initargs (h-slot-of t-association-end))))
         (other-h-init-arg (first (slot-definition-initargs (other-end-h-slot-of t-association-end)))))
    (apply 'make-instance
           h-class
           (append
            (list h-init-arg instance)
            (list other-h-init-arg other-instance)
            (when (action-slot-of t-association-end)
              (list :action action))
            (when (subtypep h-class 'temporal-object)
              (list :t-value t-value))
            (when (subtypep h-class 'time-dependent-object)
              (list :validity-start validity-start
                    :validity-end validity-end))))))
