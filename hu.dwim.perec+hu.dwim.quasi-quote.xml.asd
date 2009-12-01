;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec+hu.dwim.quasi-quote.xml
  :class hu.dwim.system
  :depends-on (:hu.dwim.perec
               :hu.dwim.quasi-quote.xml)
  :components ((:module "integration"
                :components ((:file "quasi-quote.xml")))))
