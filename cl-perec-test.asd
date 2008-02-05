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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system :cl-perec))

(in-package :cl-perec-system)

(setf *load-as-production-p* nil)

(defsystem :cl-perec-test
  :description "Tests for cl-perec."
  :depends-on (:iterate
               :arnesi
               :metabang-bind
               :defclass-star
               :local-time
               :stefil
               :cl-rdbms
               :cl-perec
               :contextl
               :metacopy)
  :default-component-class local-cl-source-file
  :components
  ((:module :test
	    :components
            ((:file "package")
             (:file "suite" :depends-on ("package"))
             (:module "persistence"
                      :depends-on ("suite")
                      :components
                      ((:file "table")
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
             (:module "tesites"
                      :depends-on ("suite")
                      :components
                      ((:file "partial-timestamp")
                       (:file "values-having-validity")
                       (:file "temporal")
                       (:file "time-dependent")
                       (:file "tesites")
                       (:file "1-n-association")))
             (:module "query"
                      :depends-on ("suite")
                      :components
                      ((:file "suite")
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
                       (:file "embedded-sql" :depends-on ("suite"))))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-perec-test))))
  (in-package :cl-perec-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (warn "Pushed :debug in *features*, set (declaim (optimize (debug 3))), set *database* and *compiled-query-cache*.")
  (eval (let ((*package* (find-package :cl-perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))"))))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-perec-test))))
  nil)
