;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defvar *compiled-query-cache* (make-hash-table :test #'equal))

(defmethod execute-query (query &rest lexical-variable-values)
  (let ((compiled-query (get-compiled-query query)))
    (apply compiled-query lexical-variable-values)))

(defun clear-compiled-query-cache ()
  (clrhash *compiled-query-cache*))

(defun get-compiled-query (query)
  (let ((compiled-query (gethash (query-hash-key-for query) *compiled-query-cache*)))
    (when (not compiled-query)
      ;; TODO: the query is copied here because the caller can change it
      ;;       the change should remove the query from the cache instead
      (setf query (copy-query query))
      (setf compiled-query (compute-as* (:kind computed-class::standalone) (eval (compile-query query))))
      (setf (gethash (query-hash-key-for query) *compiled-query-cache*) compiled-query))
    (computed-state-value compiled-query)))

(defun remove-compiled-query (query)
  (remhash (query-hash-key-for query) *compiled-query-cache*))
