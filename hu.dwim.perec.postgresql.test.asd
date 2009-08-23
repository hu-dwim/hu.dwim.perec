;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.postgresql.test
  :class hu.dwim.test-system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Test suite for hu.dwim.perec with Postgresql backend"
  :depends-on (:hu.dwim.perec.test
               :hu.dwim.perec.postgresql))

(defmethod perform :after ((o load-op) (c (eql (find-system :hu.dwim.perec.postgresql.test))))
  (let ((*package* (find-package :hu.dwim.perec.test)))
    (eval (read-from-string
           "(setf *database*
                  (make-instance 'postgresql/perec
                                 :generated-transaction-class-name 'transaction
                                 :default-result-type 'vector
                                 :muffle-warnings t
                                 :connection-specification *test-database-connection-specification*))"))
    (warn "The global binding of *database* was set according to the test setup.")))
