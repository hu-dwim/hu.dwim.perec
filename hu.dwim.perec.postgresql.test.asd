;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec.postgresql.test
  :class hu.dwim.test-system
  :setup-readtable-function-name "hu.dwim.perec::setup-readtable"
  :package-name :hu.dwim.perec.test
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.perec with Postgresql backend"
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.postgresql)
  :components ((:module "test"
                :components ((:module "backend"
                              :components ((:file "postgresql")))))))
