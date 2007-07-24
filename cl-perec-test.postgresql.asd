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

(defsystem :cl-perec-test.postgresql
  :description "Tests for cl-perec with Postgresql backend."
  :depends-on (:cl-perec-test))

(defmethod perform ((o load-op) (c (eql (find-system :cl-perec-test.postgresql))))
  (eval (read-from-string
         "(progn
            (setf *database*
                  (make-instance 'postgresql-postmodern
                                 :generated-transaction-class-name 'transaction
                                 :default-result-type 'vector
                                 :muffle-warnings t
                                 :transaction-mixin 'transaction-mixin
                                 :connection-specification cl-perec-system::*test-database-connection-specification*)))")))

(defmethod perform ((o test-op) (c (eql (find-system :cl-perec-test.postgresql))))
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'test)")))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-perec-test.postgresql))))
  nil)
