(in-package :cl-perec-test)

(defsuite* (test/persistence/class-t :in test/persistence))

(deftest test/persistence/class-t/partial-date/first-day ()
  (is (equal "2006-01-01" (prc::date-of-first-day-for-partial-date "2006")))
  (is (equal "2006-06-01" (prc::date-of-first-day-for-partial-date "2006-06")))
  (is (equal "2006-08-12" (prc::date-of-first-day-for-partial-date "2006-08-12"))))

(deftest test/persistence/class-t/partial-date/last-day ()
  (is (equal "2006-12-31" (prc::date-of-last-day-for-partial-date "2006")))
  (is (equal "2006-06-30" (prc::date-of-last-day-for-partial-date "2006-06")))
  (is (equal "2005-02-28" (prc::date-of-last-day-for-partial-date "2005-02")))
  (is (equal "2004-02-29" (prc::date-of-last-day-for-partial-date "2004-02")))
  (is (equal "2006-08-12" (prc::date-of-last-day-for-partial-date "2006-08-12"))))

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

(defun xxx ()
  (with-t "2007-01-02"))

(deftest test-1 ()
  (with-transaction
    (bind ((name "The name")
           (test (make-instance 'test)))
      ;; normal slot
      (setf (name-of test) name)
      (prc::bind-cartesian-product* ((*t* (parse-datestring "2007-01-01") (parse-datestring "2007-01-02"))
                                     ((*validity-start* *validity-end*)
                                      (list (parse-datestring "2007-01-03") (parse-datestring "2007-01-04"))
                                      (list (parse-datestring "2007-01-05") (parse-datestring "2007-01-06"))))
        (is (equal (name-of test) name)))
      ;; temporal slot
      (prc::bind-cartesian-product* (((*validity-start* *validity-end*)
                                      (list (parse-datestring "2007-01-03") (parse-datestring "2007-01-04"))
                                      (list (parse-datestring "2007-01-05") (parse-datestring "2007-01-06"))))
        (signals error (open-date-of test))
        (with-t "2007-01-02"
          (setf (open-date-of test) "2007-01-05"))
        (with-t "2007-01-04"
          (setf (open-date-of test) "2007-01-06"))
        (with-t "2007-01-01"
          (signals error (open-date-of test)))
        (with-t "2007-01-02"
          (is (local-time= (open-date-of test) "2007-01-05")))
        (with-t "2007-01-03"
          (is (local-time= (open-date-of test) "2007-01-05")))
        (with-t "2007-01-04"
          (is (local-time= (open-date-of test) "2007-01-06")))
        (with-t "2007-01-05"
          (is (local-time= (open-date-of test) "2007-01-06"))))
      ;; temporal time-depenent slot
      (with-t "2007-01-02"
        (with-validity-range "2007-01-02" "2007-01-03"
          (setf (population-of test) 100))
        (with-validity-range "2007-01-04" "2007-01-05"
          (setf (population-of test) 200)))
      (with-t "2007-01-03"
        (with-validity-range "2007-01-03" "2007-01-04"
          (setf (population-of test) 120)
          (setf (population-of test) 150)))
      (with-t "2007-01-01"
        (with-validity-range "2007-01-02" "2007-01-02"
          (signals error (population-of test))))
      (with-t "2007-01-02"
        (with-validity-range "2007-01-02" "2007-01-05"
          (let ((population (population-of test)))
            (is (= 2 (length (values-of population))))
            (iter (for (value validity-start validity-end index) :in-values-having-validity population)
                  (ecase index
                    (0 (is (and (= value 100)
                                (local-time= validity-start "2007-01-02")
                                (local-time= validity-end "2007-01-03"))))
                    (1 (is (and (= value 200)
                                (local-time= validity-start "2007-01-04")
                                (local-time= validity-end "2007-01-05")))))))))
      (with-t "2007-01-04"
        (with-validity-range "2007-01-02" "2007-01-02"
          (is (= (population-of test) 100)))
        (with-validity-range "2007-01-03" "2007-01-04"
          (is (= (population-of test) 150)))
        (with-validity-range "2007-01-05" "2007-01-05"
          (is (= (population-of test) 200))))
      ;; temporal time-dependent 1-n association slot
      #+nil
      (with-t "2007-01-02"
        (with-validity-range "2007-01-02" "2007-01-05"
          (setf (iskolák-of önkormányzat) (list test))))
      #+nil
      (with-t "2007-01-03"
        (with-validity-range "2007-01-03" "2007-01-04"
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

;; TODO: parent slot should be lied
(defpclass* xxx-parent-test ()
  ((children :type (set xxx-child-test) :persistent #f :temporal #t :time-dependent #t :association #t))
  (:metaclass persistent-class-t))

;; TODO: parent slot should be lied
(defpclass* xxx-child-test ()
  ((parent :type xxx-parent-test :persistent #f :temporal #t :time-dependent #t :association #t))
  (:metaclass persistent-class-t))

(defpclass* xxx-parent-test~xxx-child-test~t ()
  ((t-value :type timestamp)
   (validity-start :type date)
   (validity-end :type date)
   (action :type integer-16)))

(defassociation*
  ((:class xxx-parent-test :slot xxx-parent-test~xxx-child-test~ts :type (set xxx-parent-test~xxx-child-test~t))
   (:class xxx-parent-test~xxx-child-test~t :slot xxx-parent-test :type xxx-parent-test)))

(defassociation*
  ((:class xxx-child-test :slot xxx-parent-test~xxx-child-test~ts :type (set xxx-parent-test~xxx-child-test~t))
   (:class xxx-parent-test~xxx-child-test~t :slot xxx-child-test :type xxx-child-test)))

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
          #+nil (insert-item (children-of* parent) child-5)
          (insert-t-association-delta-records parent (find-slot 'xxx-parent-test 'children) (list child-5) +t-insert+)
          #+nil (delete-item (children-of* parent) child-1)
          (insert-t-association-delta-records parent (find-slot 'xxx-parent-test 'children) (list child-1) +t-delete+))
        (with-validity-range "2007-01-01" "2007-01-01"
          (is (equal (list child-2 child-1) (children-of parent))))
        (with-validity-range "2007-01-04" "2007-01-04"
          (is (equal (list child-4 child-3) (children-of parent))))
        (with-validity-range "2007-01-01" "2007-01-04"
          (let ((children (children-of parent)))
            (is (= 4 (length (values-of children))))
            (iter (for (value validity-start validity-end index) :in-values-having-validity children)
                  (ecase index
                    (0 (is (and (equal (list child-2 child-1) value)
                                (local-time= validity-start (parse-datestring "2007-01-01"))
                                (local-time= validity-end (parse-datestring "2007-01-01")))))
                    (1 (is (and (equal (list child-5 child-2) value)
                                (local-time= validity-start (parse-datestring "2007-01-02"))
                                (local-time= validity-end (parse-datestring "2007-01-02")))))
                    (2 (is (and (equal (list child-5 child-4 child-3) value)
                                (local-time= validity-start (parse-datestring "2007-01-03"))
                                (local-time= validity-end (parse-datestring "2007-01-03")))))
                    (3 (is (and (equal (list child-4 child-3) value)
                                (local-time= validity-start (parse-datestring "2007-01-04"))
                                (local-time= validity-end (parse-datestring "2007-01-04")))))))))))))
