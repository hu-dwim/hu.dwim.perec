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

(defpackage :cl-perec-system
  (:use :cl :asdf))

(defvar *test-database-connection-specification*
  '(:host "localhost" :database "dwim" :user-name "root" :password "admin123"))

(in-package :cl-perec-system)

(defvar *load-with-debug-p* t)

(defsystem :cl-perec-test
  :description "Tests for cl-perec."
  :depends-on (:iterate
               :arnesi
               :metabang-bind
               :defclass-star
               :local-time
               :stefil
               :cl-rdbms
               :cl-perec)
  :serial t
  :components
  ((:module :test
            :serial t
	    :components
            ((:file "package")
             (:file "suite")
             (:module :persistence
                      :serial t
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
                       (:file "purge")))
             (:module :query
                      :serial t
                      :components
                      ((:file "suite")
                       (:file "forum")
                       (:file "association")
                       (:file "scroll")
                       (:file "cache")
                       (:file "order-by")
                       (:file "purge")
                       (:file "table-ref")
                       (:file "polymorph")
                       (:file "type")))))))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-perec-test))))
  (in-package :cl-perec-test)
  (pushnew :debug *features*)
  (declaim (optimize (debug 3)))
  (eval (read-from-string
         "(progn
            (setf *database*
                  (make-instance 'postgresql-pg
                                 :transaction-mixin 'cl-perec::transaction-mixin
                                 :connection-specification cl-user::*test-database-connection-specification*)))")))
