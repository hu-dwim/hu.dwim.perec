;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defvar *test-database-connection-specification*
  '(:file-name "/tmp/hu.dwim.perec.test"))

(defsystem :hu.dwim.perec.sqlite.test
  :class hu.dwim.test-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.perec with Sqlite backend"
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.sqlite))

(defmethod perform ((op load-op) (system (eql (find-system :hu.dwim.perec.sqlite.test))))
  (eval (read-from-string
         "(setf *database*
                (make-instance 'sqlite
                               :generated-transaction-class-name 'transaction
                               :default-result-type 'vector
                               :muffle-warnings t
                               :transaction-mixin 'transaction-mixin
                               :connection-specification *test-database-connection-specification*))")))
