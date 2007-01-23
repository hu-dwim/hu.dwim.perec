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
        :parse-number
        :cl-ppcre
        :cl-store
        :cl-containers
        :cl-rdbms)

  (:shadow #:log
           #:set
           #:time
           #:form
           #:transaction)

  (:shadowing-import-from :iterate
                          #:finish)
  
  (:shadowing-import-from :cl-containers
                          #:dequeue
                          #:enqueue
                          #:parent)

  (:export ;; defining persistent classes
           #:defpclass
           #:defpclass*
           #:defassociation
           #:defassociation*

           ;; persistent class meta objects
           #:persistent-class
           #:persistent-association
           #:persistent-object
           #:persistent-slot-definition
           #:persistent-direct-slot-definition
           #:persistent-effective-slot-definition

           ;; types
           #:t
           #:serialized
           #:unbound
           #:null
           #:boolean
           #:integer-16
           #:integer-32
           #:integer-64
           #:integer
           #:float-32
           #:float-64
           #:number
           #:string
           #:symbol
           #:symbol*
           #:date
           #:time
           #:timestamp
           #:duration
           #:form
           #:member
           #:set

           ;; the whole transaction API is inherited from cl-rdbms
           #:with-database
           #:with-transaction
           #:with-transaction*

           ;; managing persistent objects
           #:make-persistent
           #:make-transient
           #:revive-object

           ;; most of the collection API is inherited from cl-containers 
           #:insert-item
           #:delete-item
           #:size
           #:empty-p
           #:empty!
           #:search-for-item
           #:iterate-items
           #:list-of

           ;; cache
           #:with-caching-slot-values
           #:without-caching-slot-values
           #:with-bypassing-database-access
           #:without-bypassing-database-access))

(defpackage :cl-perec-test
  (:nicknames :prct)

  (:use :common-lisp
        :closer-mop
        :iterate
        :arnesi
        :bind
        :defclass-star
        :local-time
        :stefil
        :cl-rdbms
        :cl-perec)

  (:shadow #:parent)

  (:shadowing-import-from :cl-perec
                          #:time
                          #:form
                          #:set))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as :name "cl-perec computed class universe")

(deflogger log ())
