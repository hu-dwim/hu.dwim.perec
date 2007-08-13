;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;; TODO: naming convention is really bad in this file -t means a lot of different things, mass confusion rulez, refactoring is a must

;;;;;;;;;;;;;
;;; Constants

(define-constant +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  :test local-time=
  :documentation "All dates and timestamps for temporal and time dependent slots are equal or greater than the beginning of time.")

(define-constant +end-of-time+ (parse-timestring "3000-01-01TZ")
  :test local-time=
  :documentation "All dates and timestamps for temporal and time dependent slots are equal or less than the end of time.")

(defconstant +t-delete+ 0
  "Constant used to mark RDBMS records for association slots.")

(defconstant +t-insert+ 1
  "Constant used to mark RDBMS records for association slots.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporal and time dependent

(def (special-variable :documentation "The time machine parameter, modifications made after *T* will not be visible.")
    *t*)
(def (special-variable :documentation "When a slot value is time dependent then the approximation uses constant values given for time ranges.")
    *validity-start*)
(def (special-variable :documentation "When a slot value is time dependent then the approximation uses constant values given for time ranges.")
    *validity-end*)

(macrolet ((with-partial-date (date &body forms)
             (rebinding (date)
               `(bind ((date-string
                        (if (typep ,date 'local-time)
                            (format-datestring ,date)
                            ,date)))
                 (ecase (ecase (count #\- date-string)
                          (0 :year)
                          (1 :month)
                          (2 :day))
                   ,@forms)))))
  (defun date-of-first-day-for-partial-date (date)
    (with-partial-date date
      (:year (strcat date-string "-01-01"))
      (:month (strcat date-string "-01"))
      (:day date-string)))

  (defun date-of-last-day-for-partial-date (date)
    (with-partial-date date
      (:year (strcat date-string "-12-31"))
      (:month (let ((date-1 (parse-datestring (strcat date-string "-01")))
                    (date-2 (parse-datestring (strcat date-string "-31"))))
                (multiple-value-bind (usec sec min hour day month year day-of-week daylight-saving-time-p original-timezone)
                    (decode-local-time date-1)
                  (declare (ignore usec sec min hour day day-of-week daylight-saving-time-p original-timezone))
                  (setf date-1 (encode-local-time 0 0 0 0 1 (1+ month) year))
                  (format-datestring
                   (local-time-adjust-days date-2 (- -1 (day-of (local-time::local-time-diff date-2 date-1))))))))
      (:day date-string))))

(defmacro with-t (timestamp &body forms)
  `(let ((*t* ,(if (stringp timestamp)
                   `(load-time-value (parse-timestring ,timestamp))
                   timestamp)))
    ,@forms))

(defmacro with-validity (validity &body forms)
  (assert (not (stringp (first forms))) nil
          "Evaluating the atom ~S in the body of with-validity doesn't make too much sense, you probably would like to use with-validity-range instead"
          (first forms))
  `(let ((*validity-start* (load-time-value (parse-datestring ,(date-of-first-day-for-partial-date validity))))
         (*validity-end* (load-time-value (parse-datestring ,(date-of-last-day-for-partial-date validity)))))
    ,@forms))

(defmacro with-validity-range (start end &body forms)
  `(let ((*validity-start* (load-time-value (parse-datestring ,(date-of-first-day-for-partial-date start))))
         (*validity-end* (load-time-value (parse-datestring ,(date-of-last-day-for-partial-date end)))))
    ,@forms))

(defclass* values-having-validity ()
  ((values :type (vector t))
   (validity-starts :type (vector date))
   (validity-ends :type (vector date))))

(defprint-object (instance values-having-validity)
  (write-char #\{)
  (iter (for value :in-sequence (values-of instance))
        (unless (first-iteration-p)
          (write-string ", "))
        (write value))
  (write-char #\}))

(defmacro-clause (for variables :in-values-having-validity v)
  (with-unique-names (value values validity-starts validity-ends)
    (let ((index (or (fourth variables) (gensym "INDEX-"))))
      `(progn
        (with ,value = ,v)
        (with ,values = (values-of ,value))
        (with ,validity-starts = (validity-starts-of ,value))
        (with ,validity-ends = (validity-ends-of ,value))
        (for ,index :from 0 :below (length ,values))
        (for ,(first variables) = (aref ,values ,index))
        (for ,(second variables) = (aref ,validity-starts ,index))
        (for ,(third variables) = (aref ,validity-ends ,index))))))

;;;;;;;
;;; Mop

(defcclass* persistent-class-t (persistent-class)
  ((t-class
    (compute-as (find-class (concatenate-symbol (class-name -self-) "-t")))
    :type persistent-class)
   (t-value-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 't-value))))
    :type column)
   (validity-start-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 'validity-start))))
    :type column)
   (validity-end-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 'validity-end))))
    :type column)
   (parent-slot
    (compute-as (find-slot (t-class-of -self-) (class-name -self-)))
    :type column)
   (parent-id-column
    (compute-as (id-column-of (parent-slot-of -self-)))
    :type column)))

(defcclass* persistent-slot-definition-t (standard-slot-definition)
  ((time-dependent
    #f
    :type boolean)
   (temporal
    #f
    :type boolean)))

(defcclass* persistent-direct-slot-definition-t
    (persistent-slot-definition-t standard-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-t
    (persistent-slot-definition-t standard-effective-slot-definition)
  ((t-slot ;; TODO: naming mass confusion
    (compute-as (find-slot (t-class-of (slot-definition-class -self-)) (slot-definition-name -self-)))
    :type persistent-effective-slot-definition)))

(defcclass* persistent-association-end-slot-definition-t (persistent-slot-definition-t)
  ;; TODO: kill association?!
  ((association)))

(defcclass* persistent-association-end-direct-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-direct-slot-definition-t)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-association-end-effective-slot-definition-t
    (persistent-association-end-slot-definition-t persistent-effective-slot-definition-t)
  ((t-class
    (compute-as (t-class-of (slot-definition-class -self-))) ;; TODO: hack
    :type persistent-class)
   (table
    (compute-as (primary-table-of (t-class-of -self-)))
    :type table)
   (t-value-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 't-value))))
    :type column)
   (validity-start-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 'validity-start))))
    :type column)
   (validity-end-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 'validity-end))))
    :type column)
   (action-column
    (compute-as (first (columns-of (find-slot (t-class-of -self-) 'action))))
    :type column)
   (parent-oid-columns
    (compute-as (columns-of (find-slot (t-class-of -self-) (class-name (slot-definition-class -self-)))))
    :type list)
   (child-oid-columns
    (compute-as (columns-of (child-slot-of -self-)))
    :type list)
   (child-slot
    (compute-as (find-slot (t-class-of -self-) (slot-definition-name -self-)))
    :type list)
   (action-slot
    (compute-as (find-slot (t-class-of -self-) 'action))
    :type persistent-effective-slot-definition)))

(eval-always
  ;; TODO: kill association?
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent :association)))

(defmethod validate-superclass ((class persistent-class)
                                (superclass persistent-class-t))
  t)

(defmethod validate-superclass ((class persistent-class-t)
                                (superclass persistent-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class-t)
                                         &key instance association time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-direct-slot-definition-t))
        ((or time-dependent temporal)
         (find-class 'persistent-direct-slot-definition-t))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class-t)
                                            &key instance association time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-effective-slot-definition-t))
        ((or time-dependent temporal)
         (find-class 'persistent-effective-slot-definition-t))
        (t
         (call-next-method))))

(defmethod compute-effective-slot-definition ((class persistent-class-t)
                                              slot-name
                                              direct-slot-definitions)
  (if (some (lambda (slot)
              (typep slot 'persistent-direct-slot-definition-t))
            direct-slot-definitions)
      (bind ((standard-initargs (compute-standard-effective-slot-definition-initargs class direct-slot-definitions))
             (slot-initargs (mappend (lambda (slot-option-name)
                                       (some #L(slot-initarg-and-value !1 slot-option-name)
                                             direct-slot-definitions))
                                     '(temporal time-dependent association)))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (apply #'make-instance effective-slot-class initargs))
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot value and friends

(defcondition* unbound-slot-t (unbound-slot)
  ((t :type timestamp)
   (validity-start :type date)
   (validity-end :type date)))

(defun slot-unbound-t (instance slot validity-start validity-end)
  (error 'unbound-slot-t
         :name (slot-definition-name slot)
         :instance instance
         :t *t*
         :validity-start validity-start
         :validity-end validity-end))

(defun permute (vector indices)
  (let ((vector-copy (make-array (length vector))))
    (declare (dynamic-extent vector-copy))
    (iter (for i :from 0 :below (length vector))
          (setf (aref vector-copy (aref indices i))
                (aref vector i)))
    (replace vector vector-copy)))

(defun collect-children-having-validity (child-slot records validity-start validity-end)
  ;; records are tuples ordered by t ascending: (child-oid validity-start validity-end action)
  ;; TODO: multiple column oid
  (labels ((child-of (record)
             (restore-slot-value child-slot record 0))
           (validity-start-of (record)
             (aref record 1))
           (validity-end-of (record)
             (aref record 2))
           (action-of (record)
             (aref record 3))
           (collect-children-having-validity (records validity-start validity-end)
             (bind (set)
               (iter (for record :in-sequence records)
                     (when (and (local-time<= validity-end (validity-end-of record))
                                (local-time<= (validity-start-of record) validity-start))
                       (ecase (action-of record)
                         (#.+t-insert+
                          (pushnew (child-of record) set))
                         (#.+t-delete+
                          (deletef (child-of record) set)))))
               set)))
    (bind (validities)
      (flet ((push-validity (validity)
               (pushnew validity validities :test #'local-time=)))
        (push-validity validity-start)
        (push-validity (local-time-adjust-days validity-end 1))
        (iter (for record :in-sequence records)
              (push-validity (validity-start-of record))
              (push-validity (local-time-adjust-days (validity-end-of record) 1))))
      (setf validities (sort validities #'local-time<))
      (if (= 2 (length validities))
          (collect-children-having-validity records (first validities) (local-time-adjust-days (second validities) -1))
          (bind ((size (1- (length validities)))
                 (values (make-array size :fill-pointer 0))
                 (validity-starts (make-array size :fill-pointer 0))
                 (validity-ends (make-array size :fill-pointer 0)))
            (iter (with value = nil)
                  (with previous-value = :unbound)
                  (with previous-validity = nil)
                  (for validity :in validities)
                  (for modified-validity = (local-time-adjust-days validity -1))
                  (for index :from -1)
                  (if previous-validity
                      (progn
                        (setf value (collect-children-having-validity records previous-validity modified-validity))
                        (if (equal value previous-value)
                            (decf index)
                            (progn
                              (vector-push value values)
                              (vector-push previous-validity validity-starts)
                              (vector-push modified-validity validity-ends)
                              (setf previous-value value)
                              (setf previous-validity validity))))
                      (setf previous-validity validity)))
            (if (= 1 (length values))
                (aref values 0)
                (make-instance 'values-having-validity
                               :values values
                               :validity-starts validity-starts
                               :validity-ends validity-ends)))))))

;; TODO: this failes when multiple records are present with the same t but overlapping validity ranges
;; (ordering for t does not affect the order of records) THIS MUST BE FORBIDDEN
(defun collect-values-having-validity (instance slot t-slot records)
  (let ((size (length records)))
    (cond ((zerop size)
           (slot-unbound-t instance slot *validity-start* *validity-end*))
          ((or (= size 1)
               (not (time-dependent-p slot)))
           (let ((rdbms-values (elt-0 records)))
             (restore-slot-value t-slot rdbms-values 3)))
          (t
           ;; TODO: sort arrays
           (bind ((values (make-array 4 :adjustable #t :fill-pointer 0))
                  (validity-starts (make-array 4 :adjustable #t :fill-pointer 0))
                  (validity-ends (make-array 4 :adjustable #t :fill-pointer 0))
                  (indices (make-array 4 :adjustable #t :fill-pointer 0)))
             (labels ((push-value-having-validity (value validity-start validity-end)
                        ;;(format t "~%Push Start: ~A End: ~A Value: ~A" value validity-start validity-end)
                        (vector-push-extend value values)
                        (vector-push-extend validity-start validity-starts)
                        (vector-push-extend validity-end validity-ends)
                        (vector-push-extend (length indices) indices))
                      (%collect-values-having-validity (index validity-start validity-end)
                        ;;(format t "~%Collect Index: ~A Start: ~A End: ~A" index  validity-start validity-end)
                        (when (local-time<= validity-start validity-end)
                          (if (< index (length records))
                              (iter (for i :from index :below (length records))
                                    (for record = (elt records i))
                                    (for record-validity-start = (elt record 1))
                                    (for record-validity-end = (elt record 2))
                                    (for merged-validity-start = (local-time-max validity-start record-validity-start))
                                    (for merged-validity-end = (local-time-min validity-end record-validity-end))
                                    (when (local-time<= merged-validity-start merged-validity-end)
                                      (push-value-having-validity (restore-slot-value t-slot record 3)
                                                                  merged-validity-start
                                                                  merged-validity-end)
                                      (%collect-values-having-validity (1+ i)
                                                                       validity-start
                                                                       (local-time-adjust-days merged-validity-start -1))
                                      (%collect-values-having-validity (1+ i)
                                                                       (local-time-adjust-days merged-validity-end 1)
                                                                       validity-end)
                                      (return))
                                    (finally (slot-unbound-t instance slot validity-start validity-end)))
                              (slot-unbound-t instance slot validity-start validity-end)))))
               (%collect-values-having-validity 0 *validity-start* *validity-end*)
               (sort indices #'local-time< :key (lambda (index) (aref validity-starts index)))
               (permute values indices)
               (permute validity-starts indices)
               (permute validity-ends indices)
               (if (= 1 (length values))
                   (aref values 0)
                   (make-instance 'values-having-validity
                                  :values values
                                  :validity-starts validity-starts
                                  :validity-ends validity-ends))))))))

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  (bind ((t-slot (t-slot-of slot))
         (table (table-of t-slot))
         (columns (columns-of t-slot))
         (parent-id-column (parent-id-column-of class))
         (t-value-column (t-value-column-of class))
         (validity-start-column (validity-start-column-of class))
         (validity-end-column (validity-end-column-of class))
         (records
          ;; TODO: add row limits if not time-dependent
          ;; TODO: t-value-column is not used, so maybe omit?
          (select-records (list* t-value-column validity-start-column validity-end-column columns)
                          (list (name-of table))
                          (sql-and (id-column-matcher-where-clause instance parent-id-column)
                                   ;; TODO: hack this out
                                   (sql-is-not-null (sql-identifier :name (rdbms::name-of (first columns))))
                                   (sql-and (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                                    (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                                            (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                                    (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                            (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                                    (sql-identifier :name (rdbms::name-of validity-end-column)))))
                          (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending)))))
    (collect-values-having-validity instance slot t-slot records)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-t))
  (bind ((t-class (t-class-of class))
         (t-slot (t-slot-of slot))
         (table (table-of t-slot))
         (table-name (name-of table))
         (columns (columns-of t-slot))
         (column-count (length columns))
         (parent-id-column (parent-id-column-of class))
         (t-value-column (t-value-column-of class))
         (validity-start-column (validity-start-column-of class))
         (validity-end-column (validity-end-column-of class))
         (rdbms-values (make-array column-count))
         (update-count))
    (store-slot-value t-slot new-value rdbms-values 0)
    (setf update-count
          (update-records table-name
                          columns
                          rdbms-values
                          (sql-and (id-column-matcher-where-clause instance parent-id-column)
                                   (sql-= (sql-identifier :name (rdbms::name-of t-value-column))
                                          (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                                   (sql-= (sql-identifier :name (rdbms::name-of validity-start-column))
                                          (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
                                   (sql-= (sql-identifier :name (rdbms::name-of validity-end-column))
                                          (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f))))))
    (assert (<= update-count 1))
    (when (zerop update-count)
      (bind ((size (+ (* 2 +oid-column-count+) 3 column-count))
             (rdbms-values (make-array size))
             (index 0))
        (oid->rdbms-values* (make-class-oid (class-name t-class)) rdbms-values index)
        (incf index +oid-column-count+)
        (store-slot-value (parent-slot-of class) instance rdbms-values index)
        (incf index +oid-column-count+)
        (setf (aref rdbms-values index) (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
        (incf index)
        (setf (aref rdbms-values index) (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f)))
        (incf index)
        (setf (aref rdbms-values index) (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
        (incf index)
        (store-slot-value t-slot new-value rdbms-values index)
        (insert-record table-name
                       (append +oid-column-names+
                               (list* parent-id-column t-value-column validity-start-column validity-end-column columns))
                       rdbms-values)))
    (when (typep new-value 'persistent-object)
      (invalidate-all-cached-slots new-value))
    ;; TODO: if t-instance is cached either invalidate it or set the value on it
    new-value))

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-association-end-effective-slot-definition-t))
  (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
      (select-1-1-association-t-record instance slot)
      (select-1-n-association-t-records instance slot)))

(defun select-1-1-association-t-record (instance slot)
  (bind ((table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (parent-id-column (first (parent-oid-columns-of slot)))
         (child-oid-columns (child-oid-columns-of slot)))
    ;; TODO: reuse recursive normal slot value querying
    (collect-values-having-validity
     instance slot (child-slot-of slot)
     (select-records (append (list t-value-column validity-start-column validity-end-column)
                             child-oid-columns)
                     (list table-name)
                     (sql-and (id-column-matcher-where-clause instance parent-id-column)
                              (sql-<= (sql-identifier :name (rdbms::name-of t-value-column))
                                      (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                      (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                              (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                      (sql-identifier :name (rdbms::name-of validity-end-column))))
                     (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending))))))

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

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-association-end-effective-slot-definition-t))
  (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
      (insert-1-1-association-t-record instance slot new-value)
      ;; TODO: get first and delete those
      (insert-1-n-association-delta-t-records instance slot new-value +t-insert+)))

(defun insert-1-1-association-t-record (parent slot child)
  (bind ((t-class (t-class-of slot))
         (table (table-of slot))
         (table-name (name-of table))
         (t-value-column (t-value-column-of slot))
         (validity-start-column (validity-start-column-of slot))
         (validity-end-column (validity-end-column-of slot))
         (parent-oid-columns (parent-oid-columns-of slot))
         (child-oid-columns (child-oid-columns-of slot))
         (rdbms-values (make-array (+ (* 3 +oid-column-count+) 4)))
         (index 0))
    (oid->rdbms-values* (make-class-oid (class-name t-class)) rdbms-values index)
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
  (bind ((t-class (t-class-of slot))
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
          (oid->rdbms-values* (make-class-oid (class-name t-class)) rdbms-values index)
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
