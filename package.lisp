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
        :cl-store
        :cl-containers
        :cl-rdbms)

  (:shadow #:log
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

           #:revive-object))

(defpackage :cl-perec-test
  (:use :common-lisp
        :closer-mop
        :iterate
        :arnesi
        :bind
        :defclass-star
        :cl-rdbms
        :cl-perec))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as :name "cl-perec computed class universe")

(deflogger log ())
