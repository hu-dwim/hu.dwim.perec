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

#+nil
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
                     :slot-names '(time-dependent-sister time-dependent-brother)
                     :instance-count 2
                     :operation-count 2))

(deftest test/tesites/association/1-1/temporal-and-time-dependent ()
  (run-complex-tests :class-names '(tesites-brother-test tesites-sister-test)
                     :slot-names '(temporal-and-time-dependent-sister temporal-and-time-dependent-brother)))

(deftest test/tesites/association/1-1/store-value/2 ()
  (with-transaction
    (with-default-t
      (bind ((brother (make-instance 'tesites-brother-test))
             (sister1 (make-instance 'tesites-sister-test))
             (sister2 (make-instance 'tesites-sister-test)))
        (setf (sister-of brother) sister1)
        (setf (brother-of sister2) brother)
        (is (eq sister2 (sister-of brother)))
        (is (eq brother (brother-of sister2)))
        (is (null (brother-of sister1)))))))
