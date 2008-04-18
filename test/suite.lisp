;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defun drop-all-test-tables ()
  (with-transaction
    (mapc #L(drop-table !1 :cascade #t)
          (collect-if #L(starts-with-subseq "_" !1)
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
  ;; TODO should take care of possible remaining h-object's
  (mapc (lambda (elememnt)
          (awhen (primary-table-of elememnt)
            (invalidate-computed-slot it 'ensure-exported))
          (when (typep elememnt 'persistent-class)
            (awhen (primary-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))
            (awhen (data-view-of elememnt)
              (invalidate-computed-slot it 'ensure-exported))))
        (append (hash-table-values *persistent-classes*)
                (hash-table-values *persistent-associations*)))
  (test))

(in-root-suite)

(defsuite* test)

(defsuite* (test/persistence :in test))

(defsuite* (test/query :in test))

(defsuite* (test/tesites :in test))
