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
           #:name
           #:set
           #:time
           #:form
           #:variable
           #:transaction
           #:float-type)

  (:shadowing-import-from :iterate
                          #:finish)

  (:shadowing-import-from :cl-containers
                          #:dequeue
                          #:enqueue
                          #:parent)

  (:shadowing-import-from :flexi-streams
                          #:octets-to-string
                          #:string-to-octets)

  (:shadowing-import-from :cl-rdbms
                          #:update-index)

  (:export ;; defining persistent classes
           #:defpclass
           #:defpclass*
           #:find-class
           #:abstract-p
           #:cache-p
           #:prefetch-p
           #:index-p
           #:unique-p
           #:required-p

           ;; defining persistent associations
           #:defassociation
           #:defassociation*
           #:find-association

           ;; defining persistent types
           #:defptype
           #:find-type
           #:normalized-type-of
           #:parse-type
           #:destructure-type
           #:def-member-type

           ;; persistent class meta objects
           #:persistent-class
           #:persistent-association
           #:persistent-slot-definition
           #:persistent-direct-slot-definition
           #:persistent-effective-slot-definition
           #:persistent-association-end-slot-definition
           #:persistent-association-end-direct-slot-definition
           #:persistent-association-end-effective-slot-definition
           #:persistent-type
           #:integer-type
           #:float-type
           #:string-type
           #:text-type
           #:member-type
           #:serialized-type
           #:symbol-type

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
           #:double-float
           #:number
           #:string
           #:text
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
           #:*database*
           #:with-transaction
           #:with-transaction*
           #:*transaction*
           #:mark-transaction-for-commit-only
           #:mark-transaction-for-rollback-only
           #:transaction-timestamp
           #:transaction-mixin

           ;; managing persistent objects
           #:with-making-persistent-instances
           #:with-making-transient-instances
           #:make-persistent
           #:make-transient
           #:oid-of
           #:persistent-p
           #:created-p
           #:modified-p
           #:deleted-p
           #:object-in-transaction-p
           #:transaction-of
           #:p-eq
           #:load-object
           #:revive-object
           #:purge-object
           #:purge-objects
           #:print-persistent-object

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
           #:query
           #:query-builder
           #:select
           #:select-first-matching
           #:select-last-matching
           #:select-object
           #:select-objects
           #:select-similar-object
           #:select-similar-objects
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

           ;; turning cache on and off
           #:with-caching-slot-values
           #:without-caching-slot-values

           ;; laziness
           #:with-lazy-collections
           #:without-lazy-collections

           ;; database and cache access
           #:with-bypassing-database-access
           #:without-bypassing-database-access))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as)
