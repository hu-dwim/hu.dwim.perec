;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec.oracle.test
  :class hu.dwim.test-system
  :package-name :hu.dwim.perec.test
  :description "Test suite for hu.dwim.perec with Oracle backend."
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.oracle)
  :components ((:module "test"
                :components ((:module "backend"
                              :components ((:file "oracle")))))))
