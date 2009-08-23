;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.postgresql
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Postgresql backend for hu.dwim.perec"
  :depends-on (:hu.dwim.perec
               :hu.dwim.rdbms.postgresql)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:file "postgresql")))))))
