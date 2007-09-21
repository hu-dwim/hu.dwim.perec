(in-package :cl-perec-test)

(defsuite* (test/persistence/class-t :in test/persistence))

;;;;;;;;;;;;;;;;
;;; Partial date

(defsuite* (test/persistence/class-t/partial-date :in test/persistence/class-t))

(deftest test/persistence/class-t/partial-date/first-day ()
  (is (equal "2006-01-01" (date-of-first-day-for-partial-date "2006")))
  (is (equal "2006-06-01" (date-of-first-day-for-partial-date "2006-06")))
  (is (equal "2006-08-12" (date-of-first-day-for-partial-date "2006-08-12"))))

(deftest test/persistence/class-t/partial-date/last-day ()
  (is (equal "2006-12-31" (date-of-last-day-for-partial-date "2006")))
  (is (equal "2006-06-30" (date-of-last-day-for-partial-date "2006-06")))
  (is (equal "2005-02-28" (date-of-last-day-for-partial-date "2005-02")))
  (is (equal "2004-02-29" (date-of-last-day-for-partial-date "2004-02")))
  (is (equal "2006-08-12" (date-of-last-day-for-partial-date "2006-08-12"))))

;;;;;;;;;;;;;;;;;;
;;; Time dependent

(defsuite* (test/persistence/class-t/time-dependent :in test/persistence/class-t))

(defpclass* time-dependent-test ()
  ((population :type integer-32 :time-dependent #t))
  (:metaclass persistent-class-t))

(defpclass* time-dependent-test-t ()
  ((validity-start :type date)
   (validity-end :type date)
   (population :type (or unbound integer-32))))

(defassociation*
  ((:class time-dependent-test-t :slot time-dependent-test :type time-dependent-test)
   (:class time-dependent-test :slot time-dependent-test-ts :type (set time-dependent-test-t))))

(deftest test/persistence/class-t/time-dependent/validity-not-specified ()
  (with-transaction
    (signals unbound-slot-t (population-of (make-instance 'time-dependent-test)))))

(deftest test/persistence/class-t/time-dependent/unbound/1 ()
  (with-transaction
    (with-validity "2007-01-01"
      (signals unbound-slot-t (population-of (make-instance 'time-dependent-test))))))

(deftest test/persistence/class-t/time-dependent/unbound/2 ()
  (with-transaction
    (bind ((instance (make-instance 'time-dependent-test)))
      (with-validity-range +beginning-of-time+ "2006-12-31"
        (setf (population-of instance) 1000))
      (with-validity-range "2008-01-01" +end-of-time+
        (setf (population-of instance) 2000))
      (with-validity-range "2007-01-01" "2007-12-31"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2006-01-01" "2007-12-31"
        (signals unbound-slot-t (population-of instance)))
      (with-validity-range "2007-01-01" "2008-12-31"
        (signals unbound-slot-t (population-of instance))))))

(deftest test/persistence/class-t/time-dependent/store-value/1 ()
  (with-validity "2007-01-01"
    (with-one-and-two-transactions
        (bind ((instance (make-instance 'time-dependent-test)))
          (setf (population-of instance) 1000)
          instance)
      (is (= 1000 (population-of -instance-))))))

(deftest test/persistence/class-t/time-dependent/store-value/2 ()
  (with-validity "2007-01-01"
    (with-one-and-two-transactions
        (bind ((instance (make-instance 'time-dependent-test)))
          (setf (population-of instance) 1000)
          (is (= (update-counter-of (command-counter-of *transaction*)) 1))
          (is (= (insert-counter-of (command-counter-of *transaction*)) 2))
          instance)
      (let ((update-counter (update-counter-of (command-counter-of *transaction*)))
            (insert-counter (insert-counter-of (command-counter-of *transaction*))))
        (setf (population-of -instance-) 1000)
        (is (= (update-counter-of (command-counter-of *transaction*)) (1+ update-counter)))
        (is (= (insert-counter-of (command-counter-of *transaction*)) insert-counter))))))

(deftest test/persistence/class-t/time-dependent/store-value/3 ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'time-dependent-test)))
        (with-validity "2007"
          (setf (population-of instance) 1000))
        instance)
    (signals error
      (with-validity "2007-07"
        (setf (population-of -instance-) 1000)))))

(deftest test/persistence/class-t/time-dependent/values-having-validity/1 ()
  (with-one-and-two-transactions
      (bind ((instance (make-instance 'time-dependent-test)))
        (with-validity "2006"
          (setf (population-of instance) 2006))
        instance)
    (with-validity "2007"
      (setf (population-of -instance-) 2007))
    (with-validity-range "2006" "2007"
      (iter (for index :from 0)
            (for (value validity-start validity-end) :in-values-having-validity (population-of -instance-))
            (ecase index
              (0 (is (= 2006 value))
                 (is (local-time= (parse-datestring "2006-01-01") validity-start))
                 (is (local-time= (parse-datestring "2006-12-31") validity-end)))
              (1 (is (= 2007 value))
                 (is (local-time= (parse-datestring "2007-01-01") validity-start))
                 (is (local-time= (parse-datestring "2007-12-31") validity-end))))))))

;;;;;;;;;;;;
;;; Temporal

(defsuite* (test/persistence/class-t/temporal :in test/persistence/class-t))

(defpclass* temporal-test ()
  ((population :type integer-32 :temporal #t))
  (:metaclass persistent-class-t))

(defpclass* temporal-test-t ()
  ((t-value :type timestamp)
   (population :type (or unbound integer-32))))

(defassociation*
  ((:class temporal-test-t :slot temporal-test :type temporal-test)
   (:class temporal-test :slot temporal-ts :type (set temporal-test-t))))

(deftest test/persistence/class-t/temporal/t-not-specified ()
  (with-transaction
    (signals unbound-variable (population-of (make-instance 'temporal-test)))))

(deftest test/persistence/class-t/temporal/unbound/1 ()
  (with-transaction
    (with-default-t
      (signals unbound-slot-t (population-of (make-instance 'temporal-test))))))

(deftest test/persistence/class-t/temporal/store-value/1 ()
  (with-one-and-two-transactions
      (with-default-t
        (let ((instance (make-instance 'temporal-test)))
          (setf (population-of instance) 1000)
          instance))
    (with-default-t
      (is (= 1000 (population-of -instance-))))))

(deftest test/persistence/class-t/temporal/override-value/1 ()
  (multiple-value-bind (instance t-value)
      (with-transaction
        (with-default-t
          (let ((instance (make-instance 'temporal-test)))
            (setf (population-of instance) 1000)
            (values instance *t*))))
    (with-transaction
      (with-default-t
        (with-revived-instance instance
          (setf (population-of instance) 2000))))
    (with-transaction
      (with-default-t
        (with-revived-instance instance
          (is (= 2000 (population-of instance))))))
    (with-transaction
      (with-t t-value
        (with-revived-instance instance
          (is (= 1000 (population-of instance))))))))

;;;;;
;;; T

(defsuite* (test/persistence/class-t/t :in test/persistence/class-t))

(defpclass* t-test ()
  ((population :type integer-32 :time-dependent #t :temporal #t))
  (:metaclass persistent-class-t))

(defpclass* t-test-t ()
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date)
   (population :type (or unbound integer-32))))

(defassociation*
  ((:class t-test-t :slot t-test :type t-test)
   (:class t-test :slot t-ts :type (set t-test-t))))

(deftest test/persistence/class-t/t/t-not-specified ()
  (with-transaction
    (with-validity "2007"
      (signals unbound-variable (population-of (make-instance 't-test))))))

(deftest test/persistence/class-t/t/validity-not-specified ()
  (with-transaction
    (with-default-t
      (signals unbound-slot-t (population-of (make-instance 't-test))))))

(deftest test/persistence/class-t/t/unbound/1 ()
  (with-transaction
    (with-default-t
      (with-validity "2007"
        (signals unbound-slot-t (population-of (make-instance 't-test)))))))

















;; TODO:
#+nil
(defun test-values-having-validity ()
  (extract-values-having-validity-range
   (make-instance 'values-having-validity
                  :values (make-array 2 :initial-contents '(1000 2000))
                  :validity-starts (make-array 2 :initial-contents (list (parse-datestring "2006-01-01")
                                                                         (parse-datestring "2007-01-01")))
                  :validity-ends (make-array 2 :initial-contents (list (parse-datestring "2006-12-31")
                                                                       (parse-datestring "2007-12-31"))))
   (parse-datestring "2006-06-06")
   (parse-datestring "2007-07-07")))
