;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-perec
  (:nicknames :prc)

  (:use :common-lisp
        :cl-perec-system
        :bind
        :closer-mop
        :iterate
        :arnesi
        :defclass-star
        :computed-class
        :local-time
        :parse-number
        :metacopy-with-contextl
        :cl-def
        :cl-ppcre
        :cl-serializer
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
           #:integer-8
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
           #:with-readonly-transaction
           #:in-transaction-p
           #:call-in-transaction
           #:begin-transaction
           #:commit-transaction
           #:rollback-transaction
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
           #:make-persistent-using-class
           #:make-transient
           #:make-transient-using-class
           #:lock-instance
           #:oid-of
           #:persistent-p
           #:created-p
           #:modified-p
           #:deleted-p
           #:instance-in-transaction-p
           #:instance-in-current-transaction-p
           #:transaction-of
           #:p-eq
           #:load-instance
           #:revive-instance
           #:revive-instances
           #:with-revived-instance
           #:with-revived-instances
           #:with-reloaded-instance
           #:with-reloaded-instances
           #:purge-instance
           #:purge-instances
           #:count-instances
           #:print-persistent-instance
           #:copy-persistent-instance
           #:copy-into-transaction-cache

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
           #:select-the-only-one
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
           #:re-like
           #:avg
           #:sum
           #:volatile
           #:static
           #:sql-text

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
           #:persistent-association-t
           #:persistent-slot-definition-t
           #:persistent-direct-slot-definition-t
           #:persistent-effective-slot-definition-t
           #:persistent-association-end-slot-definition-t
           #:persistent-association-end-direct-slot-definition-t
           #:persistent-association-end-effective-slot-definition-t
           #:with-t
           #:with-default-t
           #:with-validity
           #:with-validity-range
           #:transaction-t-mixin
           #:*t*
           #:*validity-start*
           #:*validity-end*
           #:values-having-validity
           #:t-value-of
           #:validity-start-of
           #:validity-end-of
           #:+beginning-of-time+
           #:+end-of-time+
           #:t-object
           #:h-object
           #:temporal-object
           #:time-dependent-object
           #:t-object-of
           #:h-objects
           #:h-objects-of
           #:integrated-time-dependent-slot-value
           #:averaged-time-dependent-slot-value

           ;; turning cache on and off
           #:with-caching-slot-values
           #:without-caching-slot-values

           ;; laziness
           #:with-lazy-slot-value-collections
           #:without-lazy-slot-value-collections

           ;; type check
           #:with-type-checking-slot-values
           #:without-type-checking-slot-values

           ;; update optimization
           #:with-storing-equal-slot-values
           #:without-storing-equal-slot-values

           ;; export,import
           #:write-persistent-object-oid
           #:read-persistent-object-oid
           ))

(in-package :cl-perec)

(import-sql-syntax-node-names)
(import-sql-constructor-names)


(define-computed-universe compute-as)

(defun transform-function-definer-options (options)
  (if cl-perec-system:*load-as-production-p*
      options
      (remove-keywords options :inline :optimize)))

;; TODO get rid of this, use string names
(defun rdbms-name-for (name &optional thing)
  (let ((name-as-string (rdbms:rdbms-name-for name thing)))
    (aif (symbol-package name)
         (intern (string-upcase name-as-string)
                 (if (eq it #.(find-package :common-lisp))
                     (find-package :cl-rdbms)
                     it))
         (make-symbol name-as-string))))
