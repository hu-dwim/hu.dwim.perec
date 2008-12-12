;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

(in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((try (system)
           (unless (asdf:find-system system nil)
             (warn "Trying to install required dependency: ~S" system)
             (when (find-package :asdf-install)
               (funcall (read-from-string "asdf-install:install") system))
             (unless (asdf:find-system system nil)
               (error "The ~A system requires ~A." (or *compile-file-pathname* *load-pathname*) system)))
           (asdf:operate 'asdf:load-op system)))
    (try :asdf-system-connections)))

(defpackage :cl-perec-system
  (:use :cl :asdf)

  (:export
   #:*load-as-production-p*))

(in-package :cl-perec-system)

(find-system :cl-rdbms) ; make sure all the cl-rdbms systems get defined

(defvar *load-as-production-p* t)

(defclass local-cl-source-file (cl-source-file)
  ())

(defmethod perform :around ((op operation) (component local-cl-source-file))
  (let ((*features* *features*)
        (*readtable* (copy-readtable *readtable*)))
    (unless *load-as-production-p*
      (pushnew :debug *features*))
    (ignore-errors
      (let ((setup-readtable-fn (read-from-string "cl-perec::setup-readtable")))
        (funcall setup-readtable-fn)
        t))
    (call-next-method)))

(defsystem :cl-perec
  :name "CL-PEREC"
  :version "1.0"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "Public Domain"
  :description "Persistent RDBMS based CLOS."
  :default-component-class local-cl-source-file
  :depends-on (:anaphora
               :iterate
               :alexandria
               :metabang-bind
               :closer-mop
               :computed-class
               :defclass-star
               :local-time
               :parse-number
               :contextl
               :metacopy
               :babel
               :ironclad
               :cl-def
               :cl-yalog
               :cl-ppcre
               :cl-serializer
               :cl-containers
               :cl-rdbms
               :cl-syntax-sugar
               :cl-walker)
  :components
  ((:file "package")
   (:file "configuration" :depends-on ("package"))
   (:file "logging" :depends-on ("package"))
   (:module "util"
            :depends-on ("configuration" "logging")
            :components
            ((:file "duplicates")
             (:file "pattern-matcher")
             (:file "logic")))
   (:module "persistence"
            :depends-on ("util")
            :components
            ((:file "api")
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
            :components
            ((:file "conditions")
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
             (:file "compiler" :depends-on ("type" "copy" "plan" "macro"))))
   (:module "dimensional"
            :depends-on ("persistence" "query")
            :components
            ((:file "common")
             (:file "dimension")
             (:file "type")
             (:file "value" :depends-on ("dimension"))
             (:file "class" :depends-on ("type" "dimension"))
             (:file "association" :depends-on ("class" "value"))
             (:file "mop" :depends-on ("class" "association"))
             (:file "object" :depends-on ("class"))
             (:file "store" :depends-on ("value" "class" "association" "object" "association-end-set"))
             (:file "cache" :depends-on ("value" "class" "association"))
             (:file "slot-value" :depends-on ("store" "association" "cache"))
             (:file "transformer" :depends-on ("type"))
             (:file "association-end-set" :depends-on ("value"))
             (:file "instance-cache" :depends-on ("slot-value"))))))

(defsystem-connection cl-perec-and-swank
  :requires (:cl-perec :swank #:cl-syntax-sugar-and-swank)
  :components
  ((:module "integration"
            :components ((:file "swank-integration")))))

(defmethod perform ((op test-op) (system (eql (find-system :cl-perec))))
  (operate 'load-op :cl-perec-test)
  (in-package :cl-perec-test)
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'retest)")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-perec))))
  nil)
