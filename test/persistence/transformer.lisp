;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* (test/persistence/transformer :in test/persistence))

(defparameter +transformer-string-test-value+ "value")

(defparameter +transformer-integer-test-value+ 42)

(defun is-equal-using-writer (type slot-value rdbms-value)
  (is (equalp rdbms-value
              (funcall (prc::compute-writer nil type) slot-value))))

(defun is-equal-using-reader (type slot-value rdbms-values)
  (is (equalp slot-value
              (funcall (prc::compute-reader nil type) rdbms-values))))

(defun is-equal-using-transformers (type slot-value rdbms-values)
  (is-equal-using-reader type slot-value rdbms-values)
  (is-equal-using-writer type slot-value rdbms-values))

;;;;;;;;;;;
;;; Boolean

(deftest test/persistence/transformer/boolean/t ()
  (is-equal-using-transformers 'boolean
                               #t
                               (list "TRUE")))

(deftest test/persistence/transformer/boolean/f ()
  (is-equal-using-transformers 'boolean
                               #f
                               (list "FALSE")))

(deftest test/persistence/transformer/or-unbound-boolean/unbound ()
  (is-equal-using-transformers '(or unbound boolean)
                               prc::+unbound-slot-value+
                               (list nil)))

(deftest test/persistence/transformer/or-unbound-boolean/t ()
  (is-equal-using-transformers '(or unbound boolean)
                               #t
                               (list "TRUE")))

(deftest test/persistence/transformer/or-unbound-boolean/f ()
  (is-equal-using-transformers '(or unbound boolean)
                               #f
                               (list "FALSE")))

;;;;;;;;;;;
;;; Integer

(deftest test/persistence/transformer/integer ()
  (is-equal-using-transformers 'integer
                               +transformer-integer-test-value+
                               (list +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-null-integer/nil ()
  (is-equal-using-transformers '(or null integer)
                               nil
                               (list nil)))

(deftest test/persistence/transformer/or-null-integer/integer ()
  (is-equal-using-transformers '(or null integer)
                               +transformer-integer-test-value+
                               (list +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-unbound-integer/unbound ()
  (is-equal-using-transformers '(or unbound integer)
                               prc::+unbound-slot-value+
                               (list nil)))

(deftest test/persistence/transformer/or-unbound-integer/integer ()
  (is-equal-using-transformers '(or unbound integer)
                               +transformer-integer-test-value+
                               (list +transformer-integer-test-value+)))

(deftest test/persistence/transformer/or-unbound-null-integer/unbound ()
  (is-equal-using-transformers '(or unbound null integer)
                               prc::+unbound-slot-value+
                               (list nil nil)))

(deftest test/persistence/transformer/or-unbound-null-integer/null ()
  (is-equal-using-transformers '(or unbound null integer)
                               nil
                               (list t nil)))

(deftest test/persistence/transformer/or-unbound-null-integer/integer ()
  (is-equal-using-transformers '(or unbound null integer)
                               +transformer-integer-test-value+
                               (list t +transformer-integer-test-value+)))

;;;;;;;;;;
;;; String

(deftest test/persistence/transformer/string ()
  (is-equal-using-transformers 'string
                               +transformer-string-test-value+
                               (list +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-null-string/nil ()
  (is-equal-using-transformers '(or null string)
                               nil
                               (list nil)))

(deftest test/persistence/transformer/or-null-string/string ()
  (is-equal-using-transformers '(or null string)
                               +transformer-string-test-value+
                               (list +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-unbound-string/unbound ()
  (is-equal-using-transformers '(or unbound string)
                               prc::+unbound-slot-value+
                               (list nil)))

(deftest test/persistence/transformer/or-unbound-string/string ()
  (is-equal-using-transformers '(or unbound string)
                               +transformer-string-test-value+
                               (list +transformer-string-test-value+)))

(deftest test/persistence/transformer/or-unbound-null-string/unbound ()
  (is-equal-using-transformers '(or unbound null string)
                               prc::+unbound-slot-value+
                               (list nil nil)))

(deftest test/persistence/transformer/or-unbound-null-string/null ()
  (is-equal-using-transformers '(or unbound null string)
                               nil
                               (list t nil)))

(deftest test/persistence/transformer/or-unbound-null-string/string ()
  (is-equal-using-transformers '(or unbound null string)
                               +transformer-string-test-value+
                               (list t +transformer-string-test-value+)))

;;;;;;;;;;;
;;; Symbol

(deftest test/persistence/transformer/symbol/nil ()
  (is-equal-using-transformers 'symbol
                               nil
                               (list "COMMON-LISP::NIL")))

(deftest test/persistence/transformer/symbol/something ()
  (is-equal-using-transformers 'symbol
                               'something
                               (list "CL-PEREC-TEST::SOMETHING")))

(deftest test/persistence/transformer/or-unbound-symbol/unbound ()
  (is-equal-using-transformers '(or unbound symbol)
                               prc::+unbound-slot-value+
                               (list nil)))

(deftest test/persistence/transformer/or-unbound-symbol/nil ()
  (is-equal-using-transformers '(or unbound symbol)
                               nil
                               (list "COMMON-LISP::NIL")))

(deftest test/persistence/transformer/or-unbound-symbol/something ()
  (is-equal-using-transformers '(or unbound symbol)
                               'something
                               (list "CL-PEREC-TEST::SOMETHING")))

;;;;;
;;; t

(deftest test/persistence/transformer/t/unbound ()
  (is-equal-using-transformers t
                               prc::+unbound-slot-value+
                               (list nil)))

(deftest test/persistence/transformer/t/nil ()
  (is-equal-using-transformers t
                               nil
                               (list #(67 108 115 84 10 35 1 3 78 73 76 35 1 11 67 79 77 77 79 78 45 76 73 83 80))))

(deftest test/persistence/transformer/t/something ()
  (is-equal-using-transformers t
                               'something
                               (list #(67 108 115 84 10 5 1 9 83 79 77 69 84 72 73 78 71 5 1 13 67 76 45 80 69 82 69 67 45 84 69 83 84))))
