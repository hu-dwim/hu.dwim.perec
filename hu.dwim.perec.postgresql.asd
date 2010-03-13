;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.perec.postgresql
  :class hu.dwim.system
  :description "Postgresql backend for hu.dwim.perec."
  :depends-on (:hu.dwim.perec
               :hu.dwim.rdbms.postgresql)
  :components ((:module "source"
                :components ((:module "backend"
                              :components ((:file "postgresql")))
                             (:module "query"
                              :components ((:file "text-search")))))))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.perec.postgresql))))
  (let ((database-variable (read-from-string "hu.dwim.rdbms::*database*")))
    (unless (boundp database-variable)
      (setf (symbol-value database-variable)
            (symbol-value (read-from-string "hu.dwim.perec.test::*postgresql-database*")))))
  (eval (let ((*package* (find-package :hu.dwim.perec)))
          (read-from-string "(setf *compiled-query-cache* (make-compiled-query-cache))")))
  (warn "The global value of *compiled-query-cache* was initialized."))
