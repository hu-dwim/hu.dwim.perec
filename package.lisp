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
        :flexi-streams
        :local-time
        :parse-number
        :metacopy-with-contextl
        :cl-def
        :cl-ppcre
        :cl-store
        :cl-containers
        :cl-rdbms)

  (:shadow #:log
           #:name
           #:set
           #:time
           #:form
           #:children
           #:variable
           #:transaction
           #:rdbms-name-for ;; TODO get rid of this
           #:float-type
           #:class-name-of)

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
                          #:update-index
                          #:insert-record)

  (:export ;; defining persistent classes
           #:defpclass
           #:defpclass*
           #:find-class
           #:find-persistent-class
           #:abstract-p
           #:cache-p
           #:prefetch-p
           #:index-p
           #:unique-p
           #:required-p
           #:association-end-query-of
           #:-instance-
           #:-associated-instance-

           ;; defining persistent associations
           #:defassociation
           #:defassociation*
           #:find-association
           #:to-one-association-end-p
           #:to-many-association-end-p

           ;; defining persistent types
           #:defptype
           #:find-type
           #:normalized-type-of
           #:parse-type
           #:unparse-type
           #:primitive-type-p
           #:primitive-type-p*
           #:persistent-class-type-p
           #:persistent-class-type-p*
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
           #:unsigned-byte-vector
           #:member
           #:set
           #:disjunct-set
           #:ordered-set
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
           #:register-transaction-hook
           #:mark-transaction-for-commit-only
           #:mark-transaction-for-rollback-only
           #:transaction-timestamp
           #:transaction-mixin
           #:transaction-with-hooks-mixin
           #:before-committing-instance
           #:after-instance-committed

           ;; managing persistent instances
           #:with-making-persistent-instances
           #:with-making-transient-instances
           #:ensure-persistent
           #:ensure-transient
           #:make-persistent
           #:make-transient
           #:lock-instance
           #:oid-of
           #:persistent-p
           #:created-p
           #:modified-p
           #:deleted-p
           #:instance-in-transaction-p
           #:transaction-of
           #:p-eq
           #:load-instance
           #:revive-instance
           #:revive-instances
           #:with-revived-instance
           #:with-revived-instances
           #:with-reloaded-instance
           #:with-reloaded-instances
           #:def-singleton-persistent-instance
           #:purge-instance
           #:purge-instances
           #:print-persistent-instance

           ;; most of the collection API is inherited from cl-containers 
           #:insert-item
           #:first-item
           #:delete-item
           #:size
           #:empty-p
           #:empty!
           #:search-for-item
           #:iterate-items
           #:list-of

           ;; query
           #:query
           #:copy-query
           #:query-builder
           #:simple-query-builder
           #:select
           #:simple-select
           #:purge
           #:from
           #:where
           #:group-by
           #:having
           #:order-by
           #:select-first-matching-instance
           #:select-last-matching-instance
           #:select-instance
           #:select-instances
           #:select-similar-instance
           #:select-similar-instances
           #:make-query
           #:current-query-variable-of
           #:add-lexical-variable
           #:add-query-variable
           #:add-assert
           #:add-order-by
           #:add-collect
           #:execute-query
           #:unique
           #:flat
           #:result-set
           #:result-type-of
           #:define-query-macro
           #:like
           #:avg
           #:sum
           #:volatile
           #:static

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

           ;; temporal and time-dependent
           #:persistent-class-t
           #:with-t
           #:with-default-t
           #:with-validity
           #:with-validity-range
           #:transaction-t-mixin
           #:*t*
           #:*validity-start*
           #:*validity-end*
           #:values-having-validity
           #:+beginning-of-time+
           #:+end-of-time+

           ;; turning cache on and off
           #:with-caching-slot-values
           #:without-caching-slot-values

           ;; laziness
           #:with-lazy-collections
           #:without-lazy-collections

           ;; type check
           #:with-type-checking-slot-values
           #:without-type-checking-slot-values))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)

(define-computed-universe compute-as)

(deflogger qlog ()
  :level +warn+
  :compile-time-level +dribble+
  :appender (make-instance 'brief-stream-log-appender :stream *debug-io*))

(defun transform-function-definer-options (options)
  (if cl-perec-system:*load-with-debug-p*
      (remove-keywords options :inline :optimize)
      options))

;; TODO get rid of this, use string names
(defun rdbms-name-for (name &optional thing)
  (let ((name-as-string (rdbms:rdbms-name-for name thing)))
    (aif (symbol-package name)
         (intern (string-upcase name-as-string)
                 (if (eq it #.(find-package :common-lisp))
                     (find-package :cl-rdbms)
                     it))
         (make-symbol name-as-string))))
