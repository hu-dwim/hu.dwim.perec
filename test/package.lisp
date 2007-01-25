;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

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
