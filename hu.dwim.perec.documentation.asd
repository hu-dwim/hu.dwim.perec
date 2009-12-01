;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.perec.oracle.test
               :hu.dwim.perec.postgresql.test
               :hu.dwim.perec.sqlite.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "perec" :depends-on ("package"))))))
