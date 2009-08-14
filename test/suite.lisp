;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(defun drop-all-test-tables ()
  (with-transaction
    (mapc [drop-table !1 :cascade #t]
          (collect-if [starts-with-subseq "_" !1]
                      (list-tables)))))

(defmacro with-and-without-caching-slot-values (&body forms)
  `(progn
    (without-caching-slot-values
      ,@forms)
    (with-caching-slot-values
      ,@forms)))

(defmacro with-two-transactions (form-1 &body forms-2)
  `(let ((-instance-
          (with-transaction
            ,form-1)))
    (with-transaction
      (revive-instance -instance-)
      ,@forms-2)))

(defmacro with-one-and-two-transactions (form-1 &body forms-2)
  `(progn
    (with-transaction
      (let ((-instance- ,form-1))
        (declare (ignorable -instance-))
        ,@forms-2))
    (with-two-transactions ,form-1 ,@forms-2)))

(defun retest ()
  (drop-all-test-tables)
  (clear-compiled-query-cache)
  ;; TODO should take care of possible remaining persistent-object-hs
  (mapc (lambda (elememnt)
          (awhen (primary-table-of elememnt)
            (invalidate-computed-slot it 'ensure-exported))
          (when (typep elememnt 'persistent-class)
            (awhen (direct-instances-identity-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (direct-instances-data-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (all-instances-identity-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (all-instances-data-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))))
        (append (hash-table-values *persistent-classes*)
                (hash-table-values *persistent-associations*)))
  (test))

;; use some unnecessary explicit package prefixing for clarity
(def class* transaction-mixin/test (hu.dwim.perec:transaction-mixin)
  ())

(def class* postgresql-postmodern/test (hu.dwim.perec:database-mixin hu.dwim.rdbms:postgresql-postmodern)
  ())

(def method transaction-mixin-class list ((self postgresql-postmodern/test))
  'transaction-mixin/test)

(defsuite* (test :in root-suite))

(defsuite* (test/persistence :in test))

(defsuite* (test/query :in test))

(defsuite* (test/dimensional :in test))

;; test dimension
(def pclass* dimension-test ()
  ())

(def dimension test :type dimension-test)

(defixture test-dimension-fixture
  (with-transaction
    (purge-instances 'dimension-test)
    (make-instance 'dimension-test)
    (make-instance 'dimension-test)
    (make-instance 'dimension-test)))

(defmacro with-setup (fixture &body body)
  (if fixture
      `(progn
        (funcall ',fixture)
        ,@body)
      `(progn
        ,@body)))
