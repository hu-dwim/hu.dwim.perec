;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defvar *test-database-connection-specification*
  '(:datasource "(ADDRESS =
                   (PROTOCOL = TCP)
                   (HOST = localhost)
                   (PORT = 1521))"
    :user-name "perec-test"
    :password "test123"))

(defsystem :hu.dwim.perec.test.oracle
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.perec with Oracle backend"
  :depends-on (:hu.dwim.perec.test))

(defmethod perform ((o load-op) (c (eql (find-system :hu.dwim.perec.test.oracle))))
  (eval (read-from-string
         "(setf *database*
                (make-instance 'oracle
                               :transaction-mixin 'transaction-mixin
                               :default-result-type 'vector
                               :muffle-warnings t
                               :connection-specification *test-database-connection-specification*))")))
