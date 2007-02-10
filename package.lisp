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
        :metacopy-with-contextl
        :cl-ppcre
        :cl-store
        :cl-containers
        :cl-rdbms)

  (:shadow #:log
           #:set
           #:time
           #:form
           #:variable
           #:transaction)

  (:shadowing-import-from :iterate
                          #:finish)

  (:shadowing-import-from :cl-containers
                          #:dequeue
                          #:enqueue
                          #:parent)

  (:shadowing-import-from :cl-rdbms
                          #:update-index)

  (:export ;; defining persistent classes
           #:defpclass
           #:defpclass*
           #:defassociation
           #:defassociation*

           ;; persistent class meta objects
           #:persistent-class
           #:persistent-association
           #:persistent-slot-definition
           #:persistent-direct-slot-definition
           #:persistent-effective-slot-definition
           #:persistent-association-end-slot-definition
           #:persistent-association-end-direct-slot-definition
           #:persistent-association-end-effective-slot-definition

           ;; primitve types
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
           #:float
           #:double
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
           #:or

           ;; persistent classes, all persistent-classes are valid slot types
           #:persistent-object
           #:persistent-set

           ;; most of the transaction API is inherited from cl-rdbms
           #:with-database
           #:with-transaction
           #:with-transaction*
           #:transaction-timestamp

           ;; managing persistent objects
           #:make-persistent
           #:make-transient
           #:persistent-p
           #:object-in-transaction-p
           #:p-eq
           #:purge-object
           #:purge-objects
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

           ;; query
           #:select
           #:select-first-matching
           #:select-last-matching
           #:select-object
           #:select-objects
           #:make-query
           #:add-lexical-variable
           #:add-query-variable
           #:add-assert
           #:add-collect
           #:execute-query
           #:unique
           #:flat
           #:result-set
           #:define-query-macro
           #:like
           #:avg
           #:sum
           #:purge

           ;; scroll
           #:scroll
           #:elements
           #:page
           #:first-page!
           #:previous-page!
           #:next-page!
           #:last-page!
           #:page-count
           #:page-size
           #:element-count
           #:fixed-size-scroll
           #:simple-scroll

           ;; cache
           #:with-caching-slot-values
           #:without-caching-slot-values

           ;; laziness
           #:with-lazy-slot-values
           #:without-lazy-slot-values

           ;; database access
           #:with-bypassing-database-access
           #:without-bypassing-database-access))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as)

(deflogger log ())
