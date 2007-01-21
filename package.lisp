;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-perec
  (:nicknames :prc)

  (:use :common-lisp
        :closer-mop
        :iterate
        :arnesi
        :bind
        :s-base64
        :computed-class
        :defclass-star
        :trivial-garbage
        :flexi-streams
        :local-time
        :cl-ppcre
        :cl-store
        :cl-containers
        :cl-rdbms)

  (:shadow #:log
           #:time
           #:form
           #:transaction)

  (:shadowing-import-from :iterate
           #:finish)
  
  (:shadowing-import-from :cl-containers
           #:dequeue
           #:enqueue
           #:parent)

  (:export #:defpclass
           #:defpclass*
           
           #:persistent-class
           #:persistent-object
           #:persistent-slot-definition
           #:persistent-direct-slot-definition
           #:persistent-effective-slot-definition

           #:make-persistent
           #:make-transient

           #:revive-object))

(defpackage :cl-perec-test
  (:use :common-lisp
        :closer-mop
        :iterate
        :arnesi
        :bind
        :defclass-star
        :stefil
        :cl-rdbms
        :cl-perec))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as :name "cl-perec computed class universe")

(deflogger log ())
