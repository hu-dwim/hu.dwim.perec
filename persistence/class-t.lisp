;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;; TODO: add cache support and override make-persistent to be able to delay slot storage

;; TODO: naming convention is really bad in this file -t means a lot of different things, mass confusion rulez, refactoring is a must
;; TODO: do not use t-value for time-dependent only slots, because the number of records increases dramatically

;;;;;;;;;;;;;
;;; Constants

(def (constant e :test 'local-time=) +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  "All dates and timestamps for temporal and time dependent slots are equal or greater than the beginning of time.")

(def (constant e :test 'local-time=) +end-of-time+ (parse-timestring "3000-01-01TZ")
  "All dates and timestamps for temporal and time dependent slots are equal or less than the end of time.")

(def constant +t-delete+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 1
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

;; TODO: this should not be part of the public API
;; TODO: add a keyword argument to with-transaction instread
(defmacro with-t (timestamp &body forms)
  `(let ((*t* ,(if (stringp timestamp)
                   `(load-time-value (parse-timestring ,timestamp))
                   timestamp)))
    ,@forms))

(defmacro with-default-t (&body forms)
  `(with-t (transaction-timestamp)
    ,@forms))

(def (function e) call-with-validity-range (start end trunk)
    (bind ((*validity-start* start)
           (*validity-end* end))
      (funcall trunk)))

(defmacro with-validity (validity &body forms)
  (assert (not (stringp (first forms))) nil
          "Evaluating the atom ~S in the body of with-validity doesn't make too much sense, you probably would like to use with-validity-range instead"
          (first forms))
  `(call-with-validity-range (load-time-value (parse-datestring ,(date-of-first-day-for-partial-date validity)))
                             (load-time-value (parse-datestring ,(date-of-last-day-for-partial-date validity)))
                             (lambda ()
                               ,@forms)))

(defmacro with-validity-range (start end &body forms)
  `(call-with-validity-range ,(if (stringp start)
                                  `(load-time-value (parse-datestring ,(date-of-first-day-for-partial-date start)))
                                  start)
                             ,(if (stringp end)
                                  `(load-time-value (parse-datestring ,(date-of-last-day-for-partial-date end)))
                                  end)
                             (lambda ()
                               ,@forms)))

(defclass transaction-t-mixin ()
  ())

;; TODO: disallow changing the t parameter in a transaction
(defmethod call-in-transaction :around (database (transaction transaction-t-mixin) function)
  (declare (ignore database function))
  (with-default-t
    (call-next-method)))

(defclass* values-having-validity ()
  ((values :type (vector t))
   (validity-starts :type (vector date))
   (validity-ends :type (vector date))))

(defun make-single-element-vector (element)
  (aprog1 (make-array 1)
    (setf (aref it 0) element)))

(defun make-single-value-having-validity (value validity-start validity-end)
  (make-instance 'values-having-validity
                 :values (make-single-element-vector value)
                 :validity-starts (make-single-element-vector validity-start)
                 :validity-ends (make-single-element-vector validity-end)))

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

(defun class-name->t-class-name (class-name)
  (concatenate-symbol class-name "-t"))

(defun t-class-name->class-name (t-class-name)
  (bind ((name (symbol-name t-class-name)))
    (intern (subseq name 0 (- (length name) 2)) (symbol-package t-class-name))))

(defcclass* persistent-class-t (persistent-class)
  ((persistent-effective-slot-ts
    (compute-as (collect-if #L(typep !1 'persistent-effective-slot-definition-t) (standard-effective-slots-of -self-)))
    :type (list persistent-effective-slot-definition-t))
   (effective-slots-with-underlying-slot-access
    (compute-as (append (persistent-effective-slots-of -self-) (persistent-effective-slot-ts-of -self-)))
    :type (list standard-effective-slot-definition)
    :documentation "A list of slots that support the underlying-slot-value protocol.")
   (t-class
    (compute-as (find-class (class-name->t-class-name (class-name -self-))))
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
    :type column))
  (:documentation "A temporal slot value is cached in the underlying slot. A time dependent slot value is cached as a values-having-validity object."))

(defcclass* persistent-slot-definition-t (standard-slot-definition)
  ((persistent
    ;; TODO: remove this slot and fix mop
    )
   (prefetch
    :type boolean
    :computed-in compute-as
    :documentation "Prefetched slots are loaded from and stored into the database at once. A prefetched slot must be in a table which can be accessed using a where clause matching to the id of the instance thus it must be in a data table. The default prefetched slot semantics can be overriden on a per direct slot basis.")
   (cache
    :type boolean
    :computed-in compute-as
    :documentation "All prefetched slots are cached slots but the opposite may not be true. When a cached slot is loaded it's value will be stored in the CLOS instance for fast subsequent read operations. Also whenever a cached slot is set the value will be remembered. The default cached slot semantics can be overriden on a per direct slot basis.")
   (time-dependent
    #f
    :type boolean)
   (temporal
    #f
    :type boolean)
   (integrated-slot-name
    nil
    :type symbol)))

(defcclass* persistent-direct-slot-definition-t
    (persistent-slot-definition-t standard-direct-slot-definition)
  ()
  (:metaclass identity-preserving-class))

(defcclass* persistent-effective-slot-definition-t
    (persistent-slot-definition-t standard-effective-slot-definition)
  ((t-slot ;; TODO: naming mass confusion
    (compute-as (find-slot (t-class-of (slot-definition-class -self-)) (slot-definition-name -self-)))
    :type persistent-effective-slot-definition)
   (prefetch
    (compute-as (prefetch-p (t-slot-of -self-)))
    :documentation "The cached option is inherited from the corresponding t slot.")
   (cache
    (compute-as (cache-p (t-slot-of -self-)))
    :documentation "The cached option is inherited from the corresponding t slot.")))

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
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent :integrated-slot-name :association)))

(defmethod validate-superclass ((class persistent-class)
                                (superclass persistent-class-t))
  t)

(defmethod validate-superclass ((class persistent-class-t)
                                (superclass persistent-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class-t)
                                         &key instance persistent association time-dependent temporal integrated-slot-name &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-direct-slot-definition-t))
        ((and persistent
              (or time-dependent temporal integrated-slot-name))
         (find-class 'persistent-direct-slot-definition-t))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class-t)
                                            &key instance association time-dependent temporal integrated-slot-name &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((and association
              (or time-dependent temporal))
         (find-class 'persistent-association-end-effective-slot-definition-t))
        ((or time-dependent temporal integrated-slot-name)
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
                                       (some #L(when ;; TODO: fragile guard check for association option
                                                   (find-slot (class-of !1) slot-option-name)
                                                 (slot-initarg-and-value !1 slot-option-name))
                                             direct-slot-definitions))
                                     '(temporal time-dependent integrated-slot-name association)))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (apply #'make-instance effective-slot-class initargs))
      (call-next-method)))

(defmethod initialize-instance :after ((instance persistent-effective-slot-definition-t) &key &allow-other-keys)
  (assert (or (temporal-p instance)
              (time-dependent-p instance)
              (integrated-slot-name-of instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot value and friends

(defcondition* unbound-slot-t (unbound-slot)
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S"
                     (cell-error-name condition)
                     (unbound-slot-instance condition))
             (when (slot-boundp condition 't-value)
               (format stream " with t ~A"
                       (t-value-of condition)))
             (when (slot-boundp condition 'validity-start)
               (when (slot-boundp condition 't-value)
                 (format stream " and"))
               (format stream " with validity range ~A -> ~A"
                       (validity-start-of condition)
                       (validity-end-of condition))))))

(defun slot-unbound-t (instance slot
                       &key (t-value nil t-value-p) (validity-start nil validity-start-p) (validity-end nil validity-end-p))
  (apply 'error 'unbound-slot-t
         :name (slot-definition-name slot)
         :instance instance
         (append
          (when (or t-value-p
                    (boundp '*t*))
            (list :t-value (or t-value
                         *t*)))
          (when (or validity-start-p
                    (boundp '*validity-start*))
            (list :validity-start (or validity-start
                                      *validity-start*)))
          (when (or validity-end-p
                    (boundp '*validity-end*))
            (list :validity-end (or validity-end
                                    *validity-end*))))))

(defun permute (vector indices)
  (let ((vector-copy (make-array (length vector))))
    (declare (dynamic-extent vector-copy))
    (iter (for i :from 0 :below (length vector))
          (setf (aref vector-copy (aref indices i))
                (aref vector i)))
    (replace vector vector-copy)))

(defun day-length-for-date-range (date-1 date-2)
  (1+ (day-of (local-time::local-time-diff date-1 date-2))))

(defun integrated-time-dependent-slot-value (instance slot-name)
  (bind ((slot-values (slot-value instance slot-name)))
    (if (typep slot-values 'values-having-validity)
        (iter (for (value validity-start validity-end index) :in-values-having-validity slot-values)
              (summing (* value (day-length-for-date-range validity-end validity-start))))
        (* slot-values (day-length-for-date-range *validity-end* *validity-start*)))))

(defun (setf integrated-time-dependent-slot-value) (new-value instance slot-name)
  (setf (slot-value instance slot-name)
        (/ new-value (day-length-for-date-range *validity-end* *validity-start*))))

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
(defun collect-single-slot-values-having-validity-from-records (instance slot records t-slot value-index)
  (collect-single-slot-values-having-validity
   instance slot records
   (lambda (record)
     (elt record 0))
   (lambda (record)
     (elt record 1))
   (lambda (record)
     (restore-slot-value t-slot record value-index))))

(defun collect-single-slot-values-having-validity-from-instances (instance slot t-instances t-slot)
  (collect-single-slot-values-having-validity
   instance slot t-instances
   (lambda (t-instance)
     (validity-start-of t-instance))
   (lambda (t-instance)
     (validity-end-of t-instance))
   (lambda (t-instance)
     (underlying-slot-boundp-or-value-using-class (class-of t-instance) t-instance t-slot))))

(defun collect-multiple-slot-values-having-validity-from-records (instance slots records t-slots)
  (iter (for slot :in slots)
        (for t-slot :in t-slots)
        (for value-index :initially 2 :then (+ value-index (length (columns-of t-slot))))
        (collect (collect-single-slot-values-having-validity-from-records instance slot records t-slot value-index))))

(defun collect-multiple-slot-values-having-validity-from-instances (instance slots t-instances t-slots)
  (iter (for slot :in slots)
        (for t-slot :in t-slots)
        (collect (collect-single-slot-values-having-validity-from-instances instance slot t-instances t-slot))))

(defun collect-single-slot-values-having-validity (instance slot value-holders validity-start-function validity-end-function value-function)
  (let ((size (length value-holders)))
    (cond ((zerop size)
           (slot-unbound-t instance slot))
          ((= size 1)
           (bind ((record (elt-0 value-holders)))
             (cond ((local-time< *validity-start* (funcall validity-start-function record))
                    (slot-unbound-t instance slot
                                    :validity-start *validity-start*
                                    :validity-end (local-time-adjust-days (funcall validity-start-function record) -1)))
                   ((local-time< (funcall validity-end-function record) *validity-end*)
                    (slot-unbound-t instance slot
                                    :validity-start (local-time-adjust-days (funcall validity-end-function record) 1)
                                    :validity-end *validity-end*))
                   (t
                    (make-single-value-having-validity
                     (funcall value-function record)
                     *validity-start*
                     *validity-end*)))))
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
                          (if (< index (length value-holders))
                              (iter (for i :from index :below (length value-holders))
                                    (for record = (elt value-holders i))
                                    (for record-validity-start = (funcall validity-start-function record))
                                    (for record-validity-end = (funcall validity-end-function record))
                                    (for merged-validity-start = (local-time-max validity-start record-validity-start))
                                    (for merged-validity-end = (local-time-min validity-end record-validity-end))
                                    (when (local-time<= merged-validity-start merged-validity-end)
                                      (push-value-having-validity (funcall value-function record)
                                                                  merged-validity-start
                                                                  merged-validity-end)
                                      (%collect-values-having-validity (1+ i)
                                                                       validity-start
                                                                       (local-time-adjust-days merged-validity-start -1))
                                      (%collect-values-having-validity (1+ i)
                                                                       (local-time-adjust-days merged-validity-end 1)
                                                                       validity-end)
                                      (return))
                                    (finally (slot-unbound-t instance slot
                                                             :validity-start validity-start
                                                             :validity-end validity-end)))
                              (slot-unbound-t instance slot
                                              :validity-start validity-start
                                              :validity-end validity-end)))))
               (%collect-values-having-validity 0 *validity-start* *validity-end*)
               (sort indices #'local-time< :key (lambda (index) (aref validity-starts index)))
               (permute values indices)
               (permute validity-starts indices)
               (permute validity-ends indices)
               (make-instance 'values-having-validity
                              :values values
                              :validity-starts validity-starts
                              :validity-ends validity-ends)))))))

;; TODO: support querying and caching multiple slots at once
(defun extract-values-having-validity (values-having-validity requested-validity-start requested-validity-end)
  (bind ((validity-starts (validity-starts-of values-having-validity))
         (validity-ends (validity-ends-of values-having-validity))
         (first-validity-start (aref validity-starts 0))
         (last-validity-end (aref validity-ends (1- (length validity-ends)))))
    (if (and (local-time<= first-validity-start requested-validity-start)
             (local-time<= requested-validity-end last-validity-end))
        (bind ((values (make-array 8 :adjustable #t :fill-pointer 0))
               (validity-starts (make-array 8 :adjustable #t :fill-pointer 0))
               (validity-ends (make-array 8 :adjustable #t :fill-pointer 0))
               (result
                (make-instance 'values-having-validity
                               :values values
                               :validity-starts validity-starts
                               :validity-ends validity-ends)))
          (flet ((push-value-with-validity (value validity-start validity-end)
                   (vector-push-extend value values)
                   (vector-push-extend validity-start validity-starts)
                   (vector-push-extend validity-end validity-ends)))
            (iter (with in-requested-validity-range = #f)
                  (for (value validity-start validity-end) :in-values-having-validity values-having-validity)
                  (if in-requested-validity-range
                      (if (local-time<= validity-start requested-validity-end validity-end)
                          (progn
                            (push-value-with-validity value validity-start requested-validity-end)
                            (setf in-requested-validity-range #f))
                          (push-value-with-validity value validity-start validity-end))
                      (progn
                        (when (local-time<= validity-start requested-validity-start validity-end)
                          (push-value-with-validity value requested-validity-start validity-end)
                          (setf in-requested-validity-range #t))))))
          (values #t
                  (if (= 1 (length values))
                      (aref values 0)
                      result)))
        (values #f nil))))

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (integrated-slot-name (integrated-slot-name-of slot)))
    (assert-instance-access)
    (if integrated-slot-name
        (integrated-time-dependent-slot-value instance integrated-slot-name)
        (bind (((values slot-value-cached cached-value) (slot-value-cached-p instance slot)))
          (when (or (not persistent)
                    (and *cache-slot-values*
                         slot-value-cached))
            (if (time-dependent-p slot)
                (progn
                  *validity-start* *validity-end*
                  (when (temporal-p slot)
                    *t*)
                  (if (unbound-marker-p cached-value)
                      (slot-unbound-t instance slot)
                      (bind (((values covers-validity-range-p value)
                              (extract-values-having-validity cached-value *validity-start* *validity-end*)))
                           (if covers-validity-range-p
                               (return-from slot-value-using-class value)
                               (unless persistent
                                 (slot-unbound-t instance slot))))))
                (progn
                  *t*
                  (if (unbound-marker-p cached-value)
                      (slot-unbound-t instance slot)
                      (return-from slot-value-using-class cached-value)))))
          (restore-slot-t class instance slot)))))

(defun restore-slot-t (class instance slot)
  (bind ((time-dependent-p (time-dependent-p slot))
         (temporal-p (temporal-p slot))
         (t-slot (t-slot-of slot))
         (table (table-of t-slot))
         (columns (columns-of t-slot))
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
                      (collect-single-slot-values-having-validity-from-records instance slot records t-slot 2)))
               (values (values-of values-having-validity)))
          (if (= 1 (length values))
              (elt values 0)
              values-having-validity))
        (if (zerop (length records))
            (slot-unbound-t instance slot)
            (setf (underlying-slot-value-using-class class instance slot)
                  (restore-slot-value t-slot (elt-0 records) 0))))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance))
         (integrated-slot-name (integrated-slot-name-of slot)))
    (assert-instance-access)
    (if integrated-slot-name
        (setf (integrated-time-dependent-slot-value instance integrated-slot-name) new-value)
        (progn
          (if persistent
              (store-slot-t class instance slot new-value)
              (assert (not (underlying-slot-boundp-using-class class instance slot))))
          (when (or (not persistent)
                    (and *cache-slot-values*
                         (cache-p slot)))
            (setf (underlying-slot-value-using-class class instance slot)
                  (if (time-dependent-p slot)
                      (make-single-value-having-validity new-value *validity-start* *validity-end*)
                      new-value)))
          new-value))))

(defun store-slot-t (class instance slot value)
  (bind ((time-dependent-p (time-dependent-p slot))
         (temporal-p (temporal-p slot))
         (t-class (t-class-of class))
         (t-slot (t-slot-of slot))
         (table (table-of t-slot))
         (table-name (name-of table))
         (columns (columns-of t-slot))
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
    (store-slot-value t-slot value rdbms-values 0)
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
        (oid->rdbms-values* (make-class-oid (class-name t-class)) rdbms-values index)
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
        (store-slot-value t-slot value rdbms-values index)
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

(defmethod slot-boundp-using-class ((class persistent-class-t)
                                    (instance persistent-object)
                                    (slot persistent-effective-slot-definition-t))
  ;; TODO: cache slot values and refactor
  (when (persistent-p instance)
    (handler-case
        (slot-value-using-class class instance slot)
      (unbound-slot-t (e)
                      (declare (ignore e))
                      (return-from slot-boundp-using-class #f)))
    #t)
  #+nil
  (error "Not yet implemented"))

(defmethod slot-makunbound-using-class ((class persistent-class-t)
                                        (instance persistent-object)
                                        (slot persistent-effective-slot-definition-t))
  (error "Not yet implemented"))

(defmethod make-persistent-using-class :after ((class persistent-class-t) (instance persistent-object))
  (dolist (slot (persistent-effective-slot-ts-of class))
    (bind ((values-having-validity (underlying-slot-boundp-or-value-using-class class instance slot)))
      (unless (unbound-marker-p values-having-validity)
        (iter (for (value validity-start validity-end) :in-values-having-validity values-having-validity)
              (with-validity-range validity-start validity-end
                (store-slot-t class instance slot value)))))))

(defmethod make-transient-using-class :after ((class persistent-class-t) (instance persistent-object))
  (error "Not yet implemented"))

;;;;;;;;;;;;;;;;;;;;;
;;; Association slots 

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-association-end-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access)
    (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
        (select-1-1-association-t-record instance slot)
        (select-1-n-association-t-records instance slot))))

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

(defmethod (setf slot-value-using-class) (new-value
                                          (class persistent-class-t)
                                          (instance persistent-object)
                                          (slot persistent-association-end-effective-slot-definition-t))
  (assert-instance-slot-correspondence)
  (bind ((persistent (persistent-p instance)))
    (assert-instance-access)
    (if (eq :1 (cardinality-kind-of (child-slot-of slot)))
        (insert-1-1-association-t-record instance slot new-value)
        ;; TODO: get first and delete those
        (insert-1-n-association-delta-t-records instance slot new-value +t-insert+))))

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
