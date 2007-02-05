;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/transformer :in test/persistence))

(defparameter +transformer-test-value+ "value")

(defun is-equal-using-writer (type slot-value rdbms-value)
  (is (equal rdbms-value
             (funcall (prc::compute-writer type) slot-value))))

;;;;;;;;;;
;;; Writer

(defsuite* (test/persistence/transformer/writer :in test/persistence/transformer))

(deftest test/persistence/transformer/writer/string ()
  (is-equal-using-writer 'string
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/persistence/transformer/writer/or-null-string/nil ()
  (is-equal-using-writer '(or null string)
                         nil
                         (list nil)))

(deftest test/persistence/transformer/writer/or-null-string/string ()
  (is-equal-using-writer '(or null string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/persistence/transformer/writer/or-unbound-string/unbound ()
  (is-equal-using-writer '(or unbound string)
                         prc::+unbound-slot-value+
                         (list nil)))

(deftest test/persistence/transformer/writer/or-unbound-string/string ()
  (is-equal-using-writer '(or unbound string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/persistence/transformer/writer/or-unbound-null-string/unbound ()
  (is-equal-using-writer '(or unbound null string)
                         prc::+unbound-slot-value+
                         (list nil nil)))

(deftest test/persistence/transformer/writer/or-unbound-null-string/null ()
  (is-equal-using-writer '(or unbound null string)
                         nil
                         (list t nil)))

(deftest test/persistence/transformer/writer/or-unbound-null-string/string ()
  (is-equal-using-writer '(or unbound null string)
                         +transformer-test-value+
                         (list t +transformer-test-value+)))

;;;;;;;;;;
;;; Reader

(defsuite* (test/persistence/transformer/reader :in test/persistence/transformer))

(defun is-equal-using-reader (type slot-value rdbms-values)
  (is (equal slot-value
             (funcall (prc::compute-reader type) rdbms-values))))

(deftest test/persistence/transformer/reader/string ()
  (is-equal-using-reader 'string
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/persistence/transformer/reader/or-null-string/nil ()
  (is-equal-using-reader '(or null string)
                         nil
                         (list nil)))

(deftest test/persistence/transformer/reader/or-null-string/string ()
  (is-equal-using-reader '(or null string)
                           +transformer-test-value+
                           (list +transformer-test-value+)))

(deftest test/persistence/transformer/reader/or-unbound-string/unbound ()
  (is-equal-using-reader '(or unbound string)
                         prc::+unbound-slot-value+
                         (list nil)))

(deftest test/persistence/transformer/reader/or-unbound-string/string ()
  (is-equal-using-reader '(or unbound string)
                         +transformer-test-value+
                         (list +transformer-test-value+)))

(deftest test/persistence/transformer/reader/or-unbound-null-string/unbound ()
  (is-equal-using-reader '(or unbound null string)
                         prc::+unbound-slot-value+
                         (list nil nil)))

(deftest test/persistence/transformer/reader/or-unbound-null-string/null ()
  (is-equal-using-reader '(or unbound null string)
                         nil
                         (list t nil)))

(deftest test/persistence/transformer/reader/or-unbound-null-string/string ()
  (is-equal-using-reader '(or unbound null string)
                         +transformer-test-value+
                         (list t +transformer-test-value+)))
