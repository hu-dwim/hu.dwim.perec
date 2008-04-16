;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;;;;;;;;;
;;; 1-1 association

(defsuite* (test/tesites/association/1-1 :in test/tesites))

(defpclass* tesites-brother-test ()
  ())
   
(defpclass* tesites-sister-test ()
  ())

(defassociation*
  ((:class tesites-sister-test :slot brother :type (or null tesites-brother-test))
   (:class tesites-brother-test :slot sister :type (or null tesites-sister-test)))
  (:time-dependent #t)
  (:temporal #t))

(deftest test/tesites/association/1-1/t-not-specified ()
  (with-transaction
    (with-validity "2007"
      (signals unbound-variable (sister-of (make-instance 'tesites-brother-test))))))

(deftest test/tesites/association/1-1/initial-value/null ()
  (with-transaction
    (with-default-t
      (is (null (sister-of (make-instance 'tesites-brother-test))))
      (is (null (brother-of (make-instance 'tesites-sister-test)))))))

(deftest test/tesites/association/1-1/store-value/1 ()
  (with-transaction
    (with-validity "2007-01-01"
      (with-default-t
        (bind ((brother (make-instance 'tesites-brother-test))
               (sister (make-instance 'tesites-sister-test)))
          (setf (sister-of brother) sister)
          (is (eq sister (sister-of brother)))
          (is (eq brother (brother-of sister))))))))
