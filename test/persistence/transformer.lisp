;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/transformer :in test/persistence))

(defparameter +transformer-string-test-value+ "value")

(defparameter +transformer-integer-test-value+ 42)

(defun is-equal-using-writer (type slot-value rdbms-values)
  (let ((written-rdbms-values (make-array (length rdbms-values))))
    (funcall (prc::compute-writer nil type) slot-value written-rdbms-values 0)
    (is (equalp rdbms-values written-rdbms-values))))

(defun is-equal-using-reader (type slot-value rdbms-values)
  (is (equalp slot-value (funcall (prc::compute-reader nil type) rdbms-values 0))))

(defun is-equal-using-transformers (type slot-value rdbms-values)
  (is-equal-using-reader type slot-value rdbms-values)
  (is-equal-using-writer type slot-value rdbms-values))

;;;;;;;;;;;
;;; Boolean

(deftest test/persistence/transformer/boolean/t ()
  (is-equal-using-transformers 'boolean
                               #t
                               (vector "TRUE")))

(deftest test/persistence/transformer/boolean/f ()
  (is-equal-using-transformers 'boolean
                               #f
                               (vector "FALSE")))

(deftest test/persistence/transformer/or-unbound-boolean/unbound ()
  (is-equal-using-transformers '(or unbound boolean)
                               prc::+unbound-slot-value+
                               (vector :null)))

(deftest test/persistence/transformer/or-unbound-boolean/t ()
  (is-equal-using-transformers '(or unbound boolean)
                               #t
                               (vector "TRUE")))

(deftest test/persistence/transformer/or-unbound-boolean/f ()
  (is-equal-using-transformers '(or unbound boolean)
                               #f
                               (vector "FALSE")))

;;;;;;;;;;;
;;; Integer

(deftest test/persistence/transformer/integer ()
  (is-equal-using-transformers 'integer
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-null-integer/nil ()
  (is-equal-using-transformers '(or null integer)
                               nil
                               (vector :null)))

(deftest test/persistence/transformer/or-null-integer/integer ()
  (is-equal-using-transformers '(or null integer)
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-unbound-integer/unbound ()
  (is-equal-using-transformers '(or unbound integer)
                               prc::+unbound-slot-value+
                               (vector :null)))

(deftest test/persistence/transformer/or-unbound-integer/integer ()
  (is-equal-using-transformers '(or unbound integer)
                               +transformer-integer-test-value+
                               (vector +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-unbound-null-integer/unbound ()
  (is-equal-using-transformers '(or unbound null integer)
                               prc::+unbound-slot-value+
                               (vector #f :null)))

(deftest test/persistence/transformer/or-unbound-null-integer/null ()
  (is-equal-using-transformers '(or unbound null integer)
                               nil
                               (vector #t :null)))

(deftest test/persistence/transformer/or-unbound-null-integer/integer ()
  (is-equal-using-transformers '(or unbound null integer)
                               +transformer-integer-test-value+
                               (vector #t +transformer-integer-test-value+)))

;;;;;;;;;;
;;; String

(deftest test/persistence/transformer/string ()
  (is-equal-using-transformers 'string
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-null-string/nil ()
  (is-equal-using-transformers '(or null string)
                               nil
                               (vector :null)))

(deftest test/persistence/transformer/or-null-string/string ()
  (is-equal-using-transformers '(or null string)
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-unbound-string/unbound ()
  (is-equal-using-transformers '(or unbound string)
                               prc::+unbound-slot-value+
                               (vector :null)))

(deftest test/persistence/transformer/or-unbound-string/string ()
  (is-equal-using-transformers '(or unbound string)
                               +transformer-string-test-value+
                               (vector +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-unbound-null-string/unbound ()
  (is-equal-using-transformers '(or unbound null string)
                               prc::+unbound-slot-value+
                               (vector #f :null)))

(deftest test/persistence/transformer/or-unbound-null-string/null ()
  (is-equal-using-transformers '(or unbound null string)
                               nil
                               (vector #t :null)))

(deftest test/persistence/transformer/or-unbound-null-string/string ()
  (is-equal-using-transformers '(or unbound null string)
                               +transformer-string-test-value+
                               (vector #t +transformer-string-test-value+)))

;;;;;;;;;;;
;;; Symbol

(deftest test/persistence/transformer/symbol/nil ()
  (is-equal-using-transformers 'symbol
                               nil
                               (vector "COMMON-LISP::NIL")))

(deftest test/persistence/transformer/symbol/something ()
  (is-equal-using-transformers 'symbol
                               'something
                               (vector "CL-PEREC-TEST::SOMETHING")))

(deftest test/persistence/transformer/or-unbound-symbol/unbound ()
  (is-equal-using-transformers '(or unbound symbol)
                               prc::+unbound-slot-value+
                               (vector :null)))

(deftest test/persistence/transformer/or-unbound-symbol/nil ()
  (is-equal-using-transformers '(or unbound symbol)
                               nil
                               (vector "COMMON-LISP::NIL")))

(deftest test/persistence/transformer/or-unbound-symbol/something ()
  (is-equal-using-transformers '(or unbound symbol)
                               'something
                               (vector "CL-PEREC-TEST::SOMETHING")))

;;;;;
;;; t

(deftest test/persistence/transformer/t/unbound ()
  (is-equal-using-transformers t
                               prc::+unbound-slot-value+
                               (vector :null)))

(deftest test/persistence/transformer/t/nil ()
  (is-equal-using-transformers t
                               nil
                               (vector
                                (flexi-streams:with-output-to-sequence (stream)
                                  (cl-store:store nil stream)))))

(deftest test/persistence/transformer/t/something ()
  (is-equal-using-transformers t
                               'something
                               (vector
                                (flexi-streams:with-output-to-sequence (stream)
                                  (cl-store:store 'something stream)))))
