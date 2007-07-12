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
      (parse-timestring timestamp)
      timestamp))

(defun ensure-date (timestamp)
  (if (stringp timestamp)
      (parse-datestring timestamp)
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
   (t-column
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
  ())

(defun slot-unbound-t (instance slot)
  (error 'unbound-slot-t :name (slot-definition-name slot) :instance instance))

(defmethod slot-value-using-class ((class persistent-class-t)
                                   (instance persistent-object)
                                   (slot persistent-effective-slot-definition-t))
  (bind ((t-slot (t-slot-of slot))
         (table (table-of t-slot))
         (columns (columns-of t-slot))
         (parent-id-column (parent-id-column-of class))
         (t-column (t-column-of class))
         (validity-start-column (validity-start-column-of class))
         (validity-end-column (validity-end-column-of class))
         (records
          ;; TODO: add row limits if not time-dependent
          (select-records (list* t-column validity-start-column validity-end-column columns)
                     (list (name-of table))
                     (sql-and (id-column-matcher-where-clause instance parent-id-column)
                              ;; TODO: hack this out
                              (sql-is-not-null (sql-identifier :name (rdbms::name-of (first columns))))
                              (sql-and (sql-<= (sql-identifier :name (rdbms::name-of t-column))
                                               (sql-literal :value *t* :type (sql-timestamp-type :with-timezone #f)))
                                       (sql-<= (sql-identifier :name (rdbms::name-of validity-start-column))
                                               (sql-literal :value *validity-end* :type (sql-timestamp-type :with-timezone #f)))
                                       (sql-<= (sql-literal :value *validity-start* :type (sql-timestamp-type :with-timezone #f))
                                               (sql-identifier :name (rdbms::name-of validity-end-column)))))
                     (list (sql-sort-spec :sort-key (sql-identifier :name (rdbms::name-of t-column)) :ordering :descending))))
         (size (length records)))
    (cond ((zerop size)
           (slot-unbound-t instance slot))
          ((or (= size 1)
               (not (time-dependent-p slot)))
           (let ((rdbms-values (elt-0 records)))
             (restore-slot-value t-slot rdbms-values 3)))
          (t
           ;; TODO: sort arrays
           (bind ((values (make-array 0 :adjustable #t :fill-pointer 0))
                  (validity-starts (make-array 0 :adjustable #t :fill-pointer 0))
                  (validity-ends (make-array 0 :adjustable #t :fill-pointer 0)))
             (labels ((push-value-having-validity (value validity-start validity-end)
                        ;;(format t "~%Push Start: ~A End: ~A Value: ~A" value validity-start validity-end)
                        (vector-push-extend value values)
                        (vector-push-extend validity-start validity-starts)
                        (vector-push-extend validity-end validity-ends))
                      (collect-values-having-validity (index validity-start validity-end)
                        ;;(format t "~%Collect Index: ~A Start: ~A End: ~A" index  validity-start validity-end)
                        (when (local-time<= validity-start validity-end)
                          (if (< index (length records))
                              (iter (for i :from index :below (length records))
                                    (for record = (aref records i))
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
                                    (finally (slot-unbound-t instance slot)))
                              (slot-unbound-t instance slot)))))
               (collect-values-having-validity 0 *validity-start* *validity-end*)
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
         (t-column (t-column-of class))
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
                                   (sql-= (sql-identifier :name (rdbms::name-of t-column))
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
                               (list* parent-id-column t-column validity-start-column validity-end-column columns))
                       rdbms-values)))))

(eval-always
  (mapc #L(pushnew !1 *allowed-slot-definition-properties*) '(:temporal :time-dependent)))

;;;;;;;;
;;; Test

(use-package :stefil)

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
