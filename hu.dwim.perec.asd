;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "RDBMS based persistent CLOS, an object relational mapping (ORM)"
  :depends-on (:babel
               :cl-containers
               :cl-ppcre
               :hu.dwim.common
               :hu.dwim.def+hu.dwim.delico
               :hu.dwim.defclass-star+hu.dwim.computed-class
               :hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.logger
               :hu.dwim.rdbms
               :hu.dwim.serializer
               :hu.dwim.syntax-sugar+hu.dwim.walker
               :hu.dwim.util
               :hu.dwim.walker
               :ironclad
               :local-time
               :metacopy-with-contextl
               :parse-number)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "configuration" :depends-on ("package"))
                             (:file "logger" :depends-on ("package"))
                             (:module "util"
                              :depends-on ("configuration" "logger")
                              :components ((:file "duplicates")
                                           (:file "pattern-matcher")
                                           (:file "logic")))
                             (:module "persistence"
                              :depends-on ("util")
                              :components ((:file "api")
                                           (:file "common")
                                           (:file "transaction")
                                           (:file "oid" :depends-on ("common" "transaction"))
                                           (:file "table" :depends-on ("common" "oid"))
                                           (:file "type" :depends-on ("table" "oid"))
                                           (:file "class" :depends-on ("table" "type" "api"))
                                           (:file "standard-type" :depends-on ("type" "class"))
                                           (:file "association" :depends-on ("class"))
                                           (:file "mop" :depends-on ("class" "association"))
                                           (:file "object" :depends-on ("api" "mop" "transaction"))
                                           (:file "instance-cache")
                                           (:file "store" :depends-on ("oid" "class" "object" "standard-type"))
                                           (:file "slot-value" :depends-on ("store"))
                                           (:file "persistent" :depends-on ("oid" "slot-value"))
                                           (:file "transformer" :depends-on ("type" "standard-type"))
                                           (:file "set")
                                           (:file "association-end-set" :depends-on ("object"))
                                           (:file "copy" :depends-on ("object"))
                                           (:file "export" :depends-on ("object"))))
                             (:module "query"
                              :depends-on ("util" "persistence")
                              :components ((:file "conditions")
                                           (:file "api")
                                           (:file "copy")
                                           (:file "macro")
                                           (:file "syntax" :depends-on ("copy"))
                                           (:file "runtime" :depends-on ("syntax"))
                                           (:file "scroll")
                                           (:file "result-set" :depends-on ("scroll"))
                                           (:file "cache" :depends-on ("query"))
                                           (:file "query" :depends-on ("conditions" "syntax" "api"))
                                           (:file "type" :depends-on ("syntax" "query"))
                                           (:file "sql" :depends-on ("type"))
                                           (:file "partial-eval" :depends-on ("query"))
                                           (:file "mapping" :depends-on ("query" "sql" "partial-eval" "runtime"))
                                           (:file "plan" :depends-on ("mapping" "result-set" "runtime"))
                                           (:file "compiler" :depends-on ("type" "copy" "plan" "macro"))
                                           (:file "constraint" :depends-on ("compiler"))))
                             (:module "dimensional"
                              :depends-on ("persistence" "query")
                              :components ((:file "common")
                                           (:file "dimension")
                                           (:file "type")
                                           (:file "set")
                                           (:file "coordinate")
                                           (:file "range" :depends-on ("coordinate"))
                                           (:file "coordinate-set" :depends-on ("dimension" "range" "set"))
                                           (:file "value" :depends-on ("dimension" "set"))
                                           (:file "standard-dimension" :depends-on ("dimension" "value"))
                                           (:file "class" :depends-on ("type" "dimension"))
                                           (:file "association" :depends-on ("class" "value"))
                                           (:file "mop" :depends-on ("class" "association"))
                                           (:file "object" :depends-on ("class"))
                                           (:file "store" :depends-on ("value" "class" "association" "object" "association-end-set"))
                                           (:file "cache" :depends-on ("value" "class" "association"))
                                           (:file "slot-value" :depends-on ("store" "association" "cache"))
                                           (:file "transformer" :depends-on ("type"))
                                           (:file "association-end-set" :depends-on ("value"))
                                           (:file "instance-cache" :depends-on ("slot-value"))))))))
