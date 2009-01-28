;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(def (persistent-type e) ip-address ()
  'iolib:inet-address)

(defmapping ip-address (sql-binary-large-object-type :size 16)
  'unsigned-byte-array->ip-address-reader
  'ip-address->unsigned-byte-vector-writer)

(def (function o) unsigned-byte-array->ip-address-reader (rdbms-values index)
  (bind ((address-vector (unsigned-byte-vector->ip-address-vector-reader rdbms-values index)))
    (iolib:make-address address-vector)))

(def (function o) ip-address->unsigned-byte-vector-writer (slot-value rdbms-values index)
  (check-type slot-value iolib:inet-address)
  (typecase slot-value
    ((or iolib:ipv4-address
         iolib:ipv6-address)
     (ip-address-vector->unsigned-byte-vector-writer (net.sockets::address-name slot-value) rdbms-values index))
    (t +type-error-marker+)))
