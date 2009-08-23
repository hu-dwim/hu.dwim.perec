;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.documentation
  :class hu.dwim.documentation-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "Documentation for hu.dwim.perec"
  :depends-on (:hu.dwim.perec.oracle.test
               :hu.dwim.perec.postgresql.test
               :hu.dwim.perec.sqlite.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "package")))))
