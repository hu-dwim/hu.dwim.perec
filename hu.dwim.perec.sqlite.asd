;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.sqlite
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Sqlite backend for hu.dwim.perec"
  :depends-on (:hu.dwim.perec
               :hu.dwim.rdbms.sqlite)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:file "sqlite")))))))
