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
    :user-name "hu.dwim.perec.test"
    :password "engedjbe"))

(defsystem :hu.dwim.perec.oracle.test
  :class hu.dwim.test-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.perec with Oracle backend"
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.oracle))

(defmethod perform ((o load-op) (c (eql (find-system :hu.dwim.perec.oracle.test))))
  (eval (read-from-string
         "(setf *database*
                (make-instance 'oracle
                               :transaction-mixin 'transaction-mixin
                               :default-result-type 'vector
                               :muffle-warnings t
                               :connection-specification *test-database-connection-specification*))")))
