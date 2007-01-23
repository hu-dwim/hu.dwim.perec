;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defsuite* test)

(defun import-cl-perec-symbols ()
  (import '(cl-perec::primary-table-of
            cl-perec::primary-tables-of
            cl-perec::data-tables-of
            cl-perec::prefetched-p
            cl-perec::cached-p
            cl-perec::table-of
            cl-perec::columns-of
            cl-perec::depends-on-of
            cl-perec::depends-on-me-of)))
