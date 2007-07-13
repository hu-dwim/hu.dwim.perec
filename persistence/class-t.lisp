;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defconstant +beginning-of-time+ (parse-timestring "1000-01-01TZ"))

(defconstant +end-of-time+ (parse-timestring "3000-01-01TZ"))

(defvar *t*)

(defvar *validity-start*)

(defvar *validity-end*)

(defun ensure-timestamp (timestamp)
  (if (stringp timestamp)
      `(load-time-value (parse-timestring ,timestamp))
      timestamp))

(defun ensure-date (timestamp)
  (if (stringp timestamp)
      `(load-time-value (parse-datestring ,timestamp))
      timestamp))

(defmacro with-t (timestamp &body forms)
  `(let ((*t* ,(ensure-timestamp timestamp)))
    ,@forms))

(defmacro with-validity-range (start end &body forms)
  `(let ((*validity-start* ,(ensure-date start))
         (*validity-end* ,(ensure-date end)))
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
  ((t-slot
    (compute-as (find-slot (t-class-of (slot-definition-class -self-)) (slot-definition-name -self-)))
    :type persistent-effective-slot-definition)))

(defmethod validate-superclass ((class persistent-class)
                                (superclass persistent-class-t))
  t)

(defmethod validate-superclass ((class persistent-class-t)
                                (superclass persistent-class))
  t)

(defmethod direct-slot-definition-class ((class persistent-class-t)
                                         &key instance time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
        ((or time-dependent temporal)
         (find-class 'persistent-direct-slot-definition-t))
        (t
         (call-next-method))))

(defmethod effective-slot-definition-class ((class persistent-class-t)
                                            &key instance time-dependent temporal &allow-other-keys)
  (cond (instance
         (class-of instance))
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
                                     '(temporal time-dependent)))
             (initargs (append slot-initargs standard-initargs))
             (effective-slot-class (apply #'effective-slot-definition-class class :persistent #t initargs)))
        (apply #'make-instance effective-slot-class initargs))
      (call-next-method)))

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

(defun collect-child-oids-having-validity (records validity-start validity-end)
  ;; records are tuples ordered by t ascending: (child-oid validity-start validity-end action)
  ;; TODO: multiple column oid
  (labels ((child-oid-of (record)
             (aref record 0))
           (validity-start-of (record)
             (aref record 1))
           (validity-end-of (record)
             (aref record 2))
           (action-of (record)
             (aref record 3))
           (collect-child-oids-having-validity (records validity-start validity-end)
             (bind (set)
               (iter (for record :in-sequence records)
                     (when (and (local-time<= validity-end (validity-end-of record))
                                (local-time<= (validity-start-of record) validity-start))
                       (ecase (action-of record)
                         (:insert
                          (pushnew (child-oid-of record) set))
                         (:delete
                          (deletef (child-oid-of record) set)))))
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
          (collect-child-oids-having-validity records (first validities) (local-time-adjust-days (second validities) -1))
          (bind ((size (1- (length validities)))
                 (values (make-array size))
                 (validity-starts (make-array size))
                 (validity-ends (make-array size)))
            (iter (for validity :in validities)
                  (for previous-validity :previous validity :initially nil)
                  (for modified-validity = (local-time-adjust-days validity -1))
                  (for index :from -1)
                  (when previous-validity
                    (setf (aref values index)
                          (collect-child-oids-having-validity records previous-validity modified-validity))
                    (setf (aref validity-starts index) previous-validity)
                    (setf (aref validity-ends index) modified-validity)))
            (make-instance 'values-having-validity
                           :values values
                           :validity-starts validity-starts
                           :validity-ends validity-ends))))))

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
                          (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-value-column)) :ordering :descending))))
         (size (length records)))
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
                      (collect-values-having-validity (index validity-start validity-end)
                        ;;(format t "~%Collect Index: ~A Start: ~A End: ~A" index  validity-start validity-end)
                        (when (local-time<= validity-start validity-end)
                          (if (< index (length records))
                              (iter (for i :from index :below (length records))
                                    (for record = (elt records i))
                                    (for record-validity-start = (local-time-max validity-start (elt record 1)))
                                    (for record-validity-end = (local-time-min validity-end (elt record 2)))
                                    (when (local-time<= record-validity-start record-validity-end)
                                      (push-value-having-validity (restore-slot-value t-slot record 3)
                                                                  record-validity-start
                                                                  record-validity-end)
                                      (collect-values-having-validity (1+ i)
                                                                      validity-start
                                                                      (local-time-adjust-days record-validity-start -1))
                                      (collect-values-having-validity (1+ i)
                                                                      (local-time-adjust-days record-validity-end 1)
                                                                      validity-end)
                                      (return))
                                    (finally (slot-unbound-t instance slot validity-start validity-end)))
                              (slot-unbound-t instance slot validity-start validity-end)))))
               (collect-values-having-validity 0 *validity-start* *validity-end*)
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

(eval-always
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent)))

;;;;;;;;
;;; Test

(eval-always
  (use-package :stefil))

(defpclass* test ()
  ((name :type (or null text))
   (open-date :type date :persistent #f :temporal #t)
   (population :type integer-32 :persistent #f :temporal #t :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* test-t ()
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date)
   (open-date :type date)
   (population :type integer-32)))

(defassociation*
  ((:class test-t :slot test :type test)
   (:class test :slot t-objects :type (set test-t))))

(defmacro bind-cartesian-product* (names-values-pairs &body forms)
  (if names-values-pairs
      (bind ((names-and-values (first names-values-pairs))
             (names (first names-and-values))
             (values (rest names-and-values)))
        (cons 'progn
              (iter (for value :in values)
                    (collect `(bind ((,names ,value))
                               (bind-cartesian-product* ,(rest names-values-pairs)
                                 ,@forms))))))
      `(progn
        ,@forms)) )

(deftest test-1 ()
  (with-transaction
    (bind ((name "The name")
           (2007-01-01 (parse-datestring "2007-01-01"))
           (2007-01-02 (parse-datestring "2007-01-02"))
           (2007-01-03 (parse-datestring "2007-01-03"))
           (2007-01-04 (parse-datestring "2007-01-04"))
           (2007-01-05 (parse-datestring "2007-01-05"))
           (2007-01-06 (parse-datestring "2007-01-06"))
           (test (make-instance 'test)))
      ;; normal slot
      (setf (name-of test) name)
      (bind-cartesian-product* ((*t* 2007-01-01 2007-01-02)
                               ((*validity-start* *validity-end*) (list 2007-01-03 2007-01-04) (list 2007-01-05 2007-01-06)))
        (is (equal (name-of test) name)))
      ;; temporal slot
      (bind-cartesian-product* (((*validity-start* *validity-end*) (list 2007-01-03 2007-01-04) (list 2007-01-05 2007-01-06)))
        (signals error (open-date-of test))
        (with-t 2007-01-02
          (setf (open-date-of test) 2007-01-05))
        (with-t 2007-01-04
          (setf (open-date-of test) 2007-01-06))
        (with-t 2007-01-01
          (signals error (open-date-of test)))
        (with-t 2007-01-02
          (is (local-time= (open-date-of test) 2007-01-05)))
        (with-t 2007-01-03
          (is (local-time= (open-date-of test) 2007-01-05)))
        (with-t 2007-01-04
          (is (local-time= (open-date-of test) 2007-01-06)))
        (with-t 2007-01-05
          (is (local-time= (open-date-of test) 2007-01-06))))
      ;; temporal time-depenent slot
      (with-t 2007-01-02
        (with-validity-range 2007-01-02 2007-01-03
          (setf (population-of test) 100))
        (with-validity-range 2007-01-04 2007-01-05
          (setf (population-of test) 200)))
      (with-t 2007-01-03
        (with-validity-range 2007-01-03 2007-01-04
          (setf (population-of test) 120)
          (setf (population-of test) 150)))
      (with-t 2007-01-01
        (with-validity-range 2007-01-02 2007-01-02
          (signals error (population-of test))))
      (with-t 2007-01-02
        (with-validity-range 2007-01-02 2007-01-05
          (let ((population (population-of test)))
            (is (= 2 (length (values-of population))))
            (iter (for (value validity-start validity-end index) :in-values-having-validity population)
                  (ecase index
                    (0 (is (and (= value 100)
                                (local-time= validity-start 2007-01-02)
                                (local-time= validity-end 2007-01-03))))
                    (1 (is (and (= value 200)
                                (local-time= validity-start 2007-01-04)
                                (local-time= validity-end 2007-01-05)))))))))
      (with-t 2007-01-04
        (with-validity-range 2007-01-02 2007-01-02
          (is (= (population-of test) 100)))
        (with-validity-range 2007-01-03 2007-01-04
          (is (= (population-of test) 150)))
        (with-validity-range 2007-01-05 2007-01-05
          (is (= (population-of test) 200))))
      ;; temporal time-dependent 1-n association slot
      #+nil
      (with-t 2007-01-02
        (with-validity-range 2007-01-02 2007-01-05
          (setf (iskolák-of önkormányzat) (list test))))
      #+nil
      (with-t 2007-01-03
        (with-validity-range 2007-01-03 2007-01-04
          (setf (iskolák-of önkormányzat) nil))))))


(defpclass* lakossag ()
  ((osszesen :type integer-32)))

(defpclass* telepules ()
  ((lakossag :type lakossag :persistent #f :temporal #t :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* telepules-t ()
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date)))

(defassociation*
  ((:class telepules-t :slot telepules :type telepules)
   (:class telepules :slot t-objects :type (set telepules-t))))

(defassociation*
  ((:class telepules-t :slot lakossag :type lakossag)
   (:class lakossag :slot telepules-t :type telepules-t)))

(deftest test-2 ()
  (with-transaction
    (with-t "2007-01-01"
      (let ((telepules (make-instance 'telepules))
            (osszesen-1 (random 100000))
            (osszesen-2 (random 100000)))
        (with-validity-range "2007-01-01" "2007-01-02"
          (setf (lakossag-of telepules) (make-instance 'lakossag :osszesen osszesen-1)))
        (with-validity-range "2007-01-03" "2007-01-04"
          (setf (lakossag-of telepules) (make-instance 'lakossag :osszesen osszesen-2)))
        (with-validity-range "2007-01-01" "2007-01-01"
          (is (= osszesen-1 (osszesen-of (lakossag-of telepules)))))
        (with-validity-range "2007-01-04" "2007-01-04"
          (is (= osszesen-2 (osszesen-of (lakossag-of telepules)))))
        (with-validity-range "2007-01-01" "2007-01-04"
          (let ((lakossag (lakossag-of telepules)))
            (is (= 2 (length (values-of lakossag))))
            (iter (for (value validity-start validity-end index) :in-values-having-validity lakossag)
                  (ecase index
                    (0 (is (and (= osszesen-1 (osszesen-of value))
                                (local-time= validity-start (parse-datestring "2007-01-01"))
                                (local-time= validity-end (parse-datestring "2007-01-02")))))
                    (1 (is (and (= osszesen-2 (osszesen-of value))
                                (local-time= validity-start (parse-datestring "2007-01-03"))
                                (local-time= validity-end (parse-datestring "2007-01-04")))))))))))))

(defun report-1 ()
  (with-transaction
    (values
     (with-t "2007-01-01"
       (iter (for telepules :in (select-instances telepules))
             (collect
                 (let ((osszes-lakos
                        (osszesen-of
                         (with-validity-range "2007-01-01" "2007-01-01"
                           (lakossag-of telepules)))))
                   (list
                    (max (if (< osszes-lakos 500)
                             3000000
                             1500000)
                         (* 1380 osszes-lakos))
                    (* 515 osszes-lakos))))))
     (select-counter-of (command-counter-of *transaction*))
     (update-counter-of (command-counter-of *transaction*))
     (insert-counter-of (command-counter-of *transaction*)))))

(defpclass* xxx-parent-test ()
  ((children :type (set xxx-child-test) :persistent #f :temporal #t :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* xxx-child-test ()
  ((parent :type xxx-parent-test :persistent #f :temporal #t :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* xxx-parent-test~xxx-child-test~t ()
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date)
   (action :type (member :insert :delete))))

(defassociation*
  ((:class xxx-parent-test :slot parent-test~child-test~ts :type (set xxx-parent-test~xxx-child-test~t))
   (:class xxx-parent-test~xxx-child-test~t :slot parent-test :type xxx-parent-test)))

(defassociation*
  ((:class xxx-child-test :slot parent-test~child-test~ts :type (set xxx-parent-test~xxx-child-test~t))
   (:class xxx-parent-test~xxx-child-test~t :slot child-test :type xxx-child-test)))

(deftest test-3 ()
  (with-transaction
    (with-t "2007-01-01"
      (let ((parent (make-instance 'xxx-parent-test))
            (child-1 (make-instance 'xxx-child-test))
            (child-2 (make-instance 'xxx-child-test))
            (child-3 (make-instance 'xxx-child-test))
            (child-4 (make-instance 'xxx-child-test))
            (child-5 (make-instance 'xxx-child-test)))
        (with-validity-range "2007-01-01" "2007-01-02"
          (setf (children-of parent) (list child-1 child-2)))
        (with-validity-range "2007-01-03" "2007-01-04"
          (setf (children-of parent) (list child-3 child-4)))
        (with-validity-range "2007-01-02" "2007-01-03"
          (insert-item (children-of* parent) child-5)
          (delete-item (children-of* parent) child-1))
        (with-validity-range "2007-01-01" "2007-01-01"
          (is (equal (list child-1 child-2) (children-of parent))))
        (with-validity-range "2007-01-04" "2007-01-04"
          (is (equal (list child-3 child-4) (children-of parent))))
        (with-validity-range "2007-01-01" "2007-01-04"
          (let ((children (children-of parent)))
            (is (= 4 (length (values-of children))))
            (iter (for (value validity-start validity-end index) :in-values-having-validity children)
                  (ecase index
                    (0 (is (and (equal (list child-1 child-2) value)
                                (local-time= validity-start (parse-datestring "2007-01-01"))
                                (local-time= validity-end (parse-datestring "2007-01-01")))))
                    (1 (is (and (equal (list child-2 child-5) value)
                                (local-time= validity-start (parse-datestring "2007-01-02"))
                                (local-time= validity-end (parse-datestring "2007-01-02")))))
                    (2 (is (and (equal (list child-3 child-4 child-5) value)
                                (local-time= validity-start (parse-datestring "2007-01-03"))
                                (local-time= validity-end (parse-datestring "2007-01-03")))))
                    (3 (is (and (equal (list child-3 child-4) value)
                                (local-time= validity-start (parse-datestring "2007-01-04"))
                                (local-time= validity-end (parse-datestring "2007-01-04")))))))))))))

(defun test-4 ()
  (collect-child-oids-having-validity
   (vector (vector 1 (parse-timestring "2007-01-01") (parse-timestring "2007-01-02") :insert)
           (vector 2 (parse-timestring "2007-01-01") (parse-timestring "2007-01-02") :insert)
           (vector 3 (parse-timestring "2007-01-03") (parse-timestring "2007-01-04") :insert)
           (vector 4 (parse-timestring "2007-01-03") (parse-timestring "2007-01-04") :insert)
           (vector 1 (parse-timestring "2007-01-02") (parse-timestring "2007-01-03") :delete)
           (vector 5 (parse-timestring "2007-01-02") (parse-timestring "2007-01-03") :insert))
   (parse-timestring "2007-01-01")
   (parse-timestring "2007-01-04")))

#|
t = (now)
validity-start = 2006-10-1
validity-end = 2006-10-1

(sum (osszesen-of (lakossag-of telepules)))

telepules -> telepules-t -> lakossag
telepules -> lakossag

(select ((id-of telepules) (osszesen-of lakossag) (t-of telepules-t) (validity-start-of telepules-t) (validity-end-of telepules-t))
  (from (telepules telepules) (telepules-t telepules-t) (lakossag lakossag))
  (where (and (eq telepules (telepules-of telepules-t))
              (eq lakossag (lakossag-of telepules-t))
              (local-time<= (t-of telepules-t) *t*)
              (local-time<= (validity-start-of telepules-t) *validity-end*)
              (local-time<= *validity-start* (validity-end-of telepules-t)))))

(select ((my** (id-of telepules)) (sum (osszesen-of lakossag)))
  (from (telepules telepules) (telepules-t telepules-t) (lakossag lakossag))
  (where (and (eq telepules (telepules-of telepules-t))
              (eq lakossag (lakossag-of telepules-t))
              (local-time<= (t-of telepules-t) *t*)
              (local-time<= (validity-start-of telepules-t) *validity-end*)
              (local-time<= *validity-start* (validity-end-of telepules-t)))))

(let ((t1)
      (t2))
  (with-transaction
    (setf t1 *transaction*)
    (with-transaction
      (setf t2 *transaction*)
      (let ((instance-2 (select-object)))
        (with-using-transaction t1
          (let ((instance-1 (reload-instance instance-2)))))))))
|#
