;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.perec.postgresql
  :class hu.dwim.system
  :author ("Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>"
           "Attila Lendvai <attila.lendvai@gmail.com>")
  :licence "BSD / Public domain"
  :description "Postgresql backend for hu.dwim.perec"
  :depends-on (:hu.dwim.perec
               :hu.dwim.rdbms.postgresql)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:file "postgresql")))))))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.perec.postgresql))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.perec.test::*postgresql-database*"))))))
