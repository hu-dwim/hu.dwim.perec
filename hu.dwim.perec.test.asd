;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec.test
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.perec"
  :depends-on (:hu.dwim.perec+swank
               :hu.dwim.perec+iolib
               :hu.dwim.perec+hu.dwim.quasi-quote.xml
               :hu.dwim.util.test)
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

