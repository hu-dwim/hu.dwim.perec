;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defvar *test-database-connection-specification*
  '(:file-name "/tmp/perec-test"))

(defsystem :hu.dwim.perec.test.sqlite
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.perec with Sqlite backend"
  :depends-on (:hu.dwim.perec.test))

(defmethod perform ((op load-op) (system (eql (find-system :hu.dwim.perec.test.sqlite))))
  (eval (read-from-string
         "(setf *database*
                (make-instance 'sqlite
                               :generated-transaction-class-name 'transaction
                               :default-result-type 'vector
                               :muffle-warnings t
                               :transaction-mixin 'transaction-mixin
                               :connection-specification *test-database-connection-specification*))")))
