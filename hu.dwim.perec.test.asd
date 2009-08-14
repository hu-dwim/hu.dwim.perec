;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.test
  :class hu.dwim.test-system
  :setup-readtable-function-name "hu.dwim.perec::setup-readtable"
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.perec"
  :depends-on (:hu.dwim.def+hu.dwim.stefil
               :hu.dwim.perec+hu.dwim.syntax-sugar+swank)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:module "persistence"
                              :depends-on ("suite")
                              :components ((:file "table")
                                           (:file "transformer")
                                           (:file "transaction")
                                           (:file "persistence")
                                           (:file "canonical")
                                           (:file "type")
                                           (:file "inheritance")
                                           (:file "reference")
                                           (:file "set")
                                           (:file "1-1-association")
                                           (:file "1-n-association")
                                           (:file "m-n-association")
                                           (:file "cache")
                                           (:file "instance")
                                           (:file "purge")))
                             (:module "query"
                              :depends-on ("suite")
                              :components ((:file "suite")
                                           (:file "forum" :depends-on ("suite"))
                                           (:file "association" :depends-on ("suite"))
                                           (:file "1-1-association" :depends-on ("suite"))
                                           (:file "1-n-association" :depends-on ("suite"))
                                           (:file "m-n-association" :depends-on ("suite"))
                                           (:file "scroll" :depends-on ("suite"))
                                           (:file "cache" :depends-on ("suite"))
                                           (:file "order-by" :depends-on ("suite"))
                                           (:file "purge" :depends-on ("suite"))
                                           (:file "table-ref" :depends-on ("suite"))
                                           (:file "polymorph" :depends-on ("suite"))
                                           (:file "type" :depends-on ("suite"))
                                           (:file "aggregate" :depends-on ("suite"))
                                           (:file "group-by" :depends-on ("suite"))
                                           (:file "having" :depends-on ("suite"))
                                           (:file "partial-eval" :depends-on ("suite"))
                                           (:file "expression" :depends-on ("suite"))
                                           (:file "subselect" :depends-on ("suite"))
                                           (:file "embedded-sql" :depends-on ("suite"))
                                           (:file "limit" :depends-on ("suite"))
                                           (:file "update" :depends-on ("suite"))))
                             (:module "dimensional"
                              :depends-on ("suite")
                              :components ((:file "partial-timestamp")
                                           (:file "value")
                                           (:file "time")
                                           (:file "validity")
                                           (:file "dimensional")
                                           (:file "cache")
                                           (:file "complex")
                                           (:file "association")
                                           (:file "slot" :depends-on ("complex"))
                                           (:file "1-1-association" :depends-on ("complex" "association"))
                                           (:file "1-n-association" :depends-on ("complex" "association"))
                                           (:file "m-n-association" :depends-on ("complex" "association"))))))))

(defmethod perform :after ((op load-op) (system (eql (find-system :hu.dwim.perec.test))))
  (eval (let ((*package* (find-package :hu.dwim.perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))")))
  (warn "The global value of *compiled-query-cache* was initialized."))
