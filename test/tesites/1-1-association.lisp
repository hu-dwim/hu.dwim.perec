;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; 1-1 association

(defsuite* (test/tesites/association :in test/tesites))

(defsuite* (test/tesites/association/1-1 :in test/tesites/association))

(defpclass* tesites-brother-test ()
  ())
   
(defpclass* tesites-sister-test ()
  ())

(defassociation*
  ((:class tesites-sister-test :slot brother :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot sister :type (or null tesites-sister-test))))


(defassociation*
  ((:class tesites-sister-test :slot temporal-brother :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot temporal-sister :type (or null tesites-sister-test)))
  (:temporal #t))

(defassociation*
  ((:class tesites-sister-test :slot time-dependent-brother :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot time-dependent-sister :type (or null tesites-sister-test)))
  (:time-dependent #t))

(defassociation*
  ((:class tesites-sister-test :slot temporal-and-time-dependent-brother :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot temporal-and-time-dependent-sister :type (or null tesites-sister-test)))
  (:temporal #t)
  (:time-dependent #t))

(deftest test/tesites/association/1-1/normal ()
  (run-complex-tests :class-names '(tesites-brother-test tesites-sister-test)
                     :slot-names '(sister brother)))

(deftest test/tesites/association/1-1/temporal ()
  (run-complex-tests :class-names '(tesites-brother-test tesites-sister-test)
                    :slot-names '(temporal-sister temporal-brother)))

(deftest test/tesites/association/1-1/time-dependent ()
  (run-complex-tests :class-names '(tesites-brother-test tesites-sister-test)
                     :slot-names '(time-dependent-sister time-dependent-brother)))

(deftest test/tesites/association/1-1/temporal-and-time-dependent ()
  (run-complex-tests :class-names '(tesites-brother-test tesites-sister-test)
                     :slot-names '(temporal-and-time-dependent-sister temporal-and-time-dependent-brother)))

(deftest test/tesites/association/1-1/store-value/2 ()
  (with-transaction
    (bind ((brother (make-instance 'tesites-brother-test))
           (sister1 (make-instance 'tesites-sister-test))
           (sister2 (make-instance 'tesites-sister-test)))
      (setf (sister-of brother) sister1)
      (setf (brother-of sister2) brother)
      (is (eq sister2 (sister-of brother)))
      (is (eq brother (brother-of sister2)))
      (is (null (brother-of sister1))))))

(deftest test/tesites/association/1-1/integrity () 
  (bind ((brother-1 (with-transaction (make-instance 'tesites-brother-test)))
         (sister-1 (with-transaction (make-instance 'tesites-sister-test)))
         (sister-2 (with-transaction (make-instance 'tesites-sister-test)))
         (brother-2 (with-transaction (make-instance 'tesites-brother-test))))

    (with-transaction
      (with-revived-instances (brother-1 sister-1 sister-2 brother-2)
        (with-t "2002-01-01T00:00:00Z"
          (with-validity-range "2002-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
            (setf (slot-value sister-1 'temporal-and-time-dependent-brother) brother-1))
          ;; this clears the sister-1's slot on the intersection of this and previous interval
          (with-validity-range "2001-01-01T00:00:00Z" "2003-01-01T00:00:00Z"
            (setf (slot-value sister-2 'temporal-and-time-dependent-brother) brother-1)))
        (with-t "1000-01-01T00:00:00Z"
          (with-validity-range "2000-01-01T00:00:00Z" "2003-01-01T00:00:00Z"
            (setf (slot-value sister-1 'temporal-and-time-dependent-brother) brother-2)))))

    (with-transaction
      (with-revived-instances (sister-1 brother-1 brother-2)
        (with-t "2002-01-01T00:00:00Z"
          (with-validity-range "1000-01-01T00:00:00Z" "3000-01-01T00:00:00Z"
            (is (values-having-validity=
                 (consolidate-values-having-validity
                  (slot-value sister-1 'temporal-and-time-dependent-brother))
                 (make-values-having-validity*
                  (list (list nil "1000-01-01T00:00:00Z" "2000-01-01T00:00:00Z")
                        (list brother-2 "2000-01-01T00:00:00Z" "2002-01-01T00:00:00Z")
                        (list nil "2002-01-01T00:00:00Z" "2003-01-01T00:00:00Z")
                        (list brother-1 "2003-01-01T00:00:00Z" "3000-01-01T00:00:00Z")))))))))))
