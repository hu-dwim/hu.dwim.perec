;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* test/transformer :in test/persistence)

(defparameter +transformer-test-value+ "value")

(defun is-equal-using-writer (type slot-value rdbms-value)
  (is (equal rdbms-value
             (funcall (prc::compute-writer type) slot-value))))

;;;;;;;;;;
;;; Writer

(defsuite* test/transformer/writer :in test/transformer)

(deftest test/transformer/writer/string ()
  (is-equal-using-writer 'string
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/transformer/writer/or-null-string/nil ()
  (is-equal-using-writer '(or null string)
                         nil
                         (list nil)))

(deftest test/transformer/writer/or-null-string/string ()
  (is-equal-using-writer '(or null string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/transformer/writer/or-unbound-string/unbound ()
  (is-equal-using-writer '(or unbound string)
                         prc::+the-unbound-slot-value+
                         (list nil)))

(deftest test/transformer/writer/or-unbound-string/string ()
  (is-equal-using-writer '(or unbound string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/transformer/writer/or-unbound-null-string/unbound ()
  (is-equal-using-writer '(or unbound null string)
                         prc::+the-unbound-slot-value+
                         (list nil nil)))

(deftest test/transformer/writer/or-unbound-null-string/null ()
  (is-equal-using-writer '(or unbound null string)
                         nil
                         (list t nil)))

(deftest test/transformer/writer/or-unbound-null-string/string ()
  (is-equal-using-writer '(or unbound null string)
                         +transformer-test-value+
                         (list t +transformer-test-value+)))

;;;;;;;;;;
;;; Reader

(defsuite* test/transformer/reader :in test/transformer)

(defun is-equal-using-reader (type slot-value rdbms-values)
  (is (equal slot-value
             (funcall (prc::compute-reader type) rdbms-values))))

(deftest test/transformer/reader/string ()
  (is-equal-using-reader 'string
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/transformer/reader/or-null-string/nil ()
  (is-equal-using-reader '(or null string)
                         nil
                         (list nil)))

(deftest test/transformer/reader/or-null-string/string ()
  (is-equal-using-reader '(or null string)
                           +transformer-test-value+
                           (list +transformer-test-value+)))

(deftest test/transformer/reader/or-unbound-string/unbound ()
  (is-equal-using-reader '(or unbound string)
                         prc::+the-unbound-slot-value+
                         (list nil)))

(deftest test/transformer/reader/or-unbound-string/string ()
  (is-equal-using-reader '(or unbound string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/transformer/reader/or-unbound-null-string/unbound ()
  (is-equal-using-reader '(or unbound null string)
                         prc::+the-unbound-slot-value+
                         (list nil nil)))

(deftest test/transformer/reader/or-unbound-null-string/null ()
  (is-equal-using-reader '(or unbound null string)
                         nil
                         (list t nil)))

(deftest test/transformer/reader/or-unbound-null-string/string ()
  (is-equal-using-reader '(or unbound null string)
                         +transformer-test-value+
                         (list t +transformer-test-value+)))
