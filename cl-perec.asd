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

(in-package :cl-perec-system)

(defsystem :cl-perec
  :name "CL-PEREC"
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Tamás Borbély <tomi.borbely@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :maintainer ("Attila Lendvai <attila.lendvai@gmail.com>"
               "Tamás Borbély <tomi.borbely@gmail.com>"
	       "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "Public Domain"
  :description "Persistent RDBMS based CLOS."
  :depends-on (:iterate
               :arnesi
               :metabang-bind
               :s-base64
               :computed-class
               :defclass-star
               :trivial-garbage
               :cl-store
               :cl-containers
               :cl-rdbms)
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:module :persistence
            :components
            ((:file "oid")
             (:file "table")
             (:file "persistent-class")
             (:file "persistent-object")
             (:file "serialization")
             (:file "transaction")
             (:file "where-clause")
             (:file "object-cache")
             (:file "object")
;             (:file "transformer")
;             (:file "store")
;             (:file "slot-value")
;             (:file "column-type")
;             (:file "install-property-accessor")
             ))
   (:module :query
            :components
            ())))

(defsystem :cl-perec-test
  :description "Tests for cl-perec."
  :depends-on (:iterate
               :arnesi
               :metabang-bind
               :defclass-star
               :stefil
               :cl-rdbms
               :cl-perec)
  :components
  ((:module :test
	    :components
            ())))

(defmethod perform :after ((o load-op) (c (eql (find-system :cl-perec-test))))
  (in-package :cl-perec-test)
  (pushnew :debug *features*))

(defmethod perform ((op test-op) (system (eql (find-system :cl-perec))))
  (operate 'load-op :cl-perec-test)
  (in-package :cl-perec-test))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-perec))))
  nil)
