;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defvar *test-database-connection-specification*
  '(:host "localhost" :database "perec-test" :user-name "perec-test" :password "test123"))

(defsystem :hu.dwim.perec.test.postgresql
  :class hu.dwim.test-system
  :description "Test suite for hu.dwim.perec with Postgresql backend"
  :depends-on (:hu.dwim.perec.test))

(defmethod perform :after ((o load-op) (c (eql (find-system :hu.dwim.perec.test.postgresql))))
  (let ((*package* (find-package :hu.dwim.perec.test)))
    (eval (read-from-string
           "(setf *database*
                  (make-instance 'postgresql-postmodern/test
                                 :generated-transaction-class-name 'transaction
                                 :default-result-type 'vector
                                 :muffle-warnings t
                                 :connection-specification *test-database-connection-specification*))"))
    (warn "The global binding of *database* was set according to the test setup.")))
