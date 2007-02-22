;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(define-copy-protocol copy-query)

(define-copy-method (copy-one copy-query) ((thing t) htable)
  thing)

