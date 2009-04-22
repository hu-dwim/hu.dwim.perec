;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; API

(def (generic e) export-persistent-instances (thing format stream &key &allow-other-keys))

(def (generic e) import-persistent-instances (format stream &key &allow-other-keys))

;;;;;;
;;; Serializer

(def constant +persistent-object-code+ #x61)

(def method export-persistent-instances (thing (format (eql :binary)) stream &key (persistent-object-serializer #'write-persistent-object-slot-values) &allow-other-keys)
  (serializer:serialize thing :output stream :serializer-mapper (make-export-serializer-mapper persistent-object-serializer)))

(def method import-persistent-instances ((format (eql :binary)) stream &key (persistent-object-deserializer #'read-persistent-object-slot-values) &allow-other-keys)
  (serializer:deserialize stream :deserializer-mapper (make-export-deserializer-mapper persistent-object-deserializer)))

(def (function o) make-export-serializer-mapper (persistent-object-serializer)
  (lambda (instance context)
    (bind (((:values code has-identity writer-function)
            (cl-serializer::default-serializer-mapper instance context)))
      (if (and (eq code serializer::+standard-object-code+)
               (typep instance 'persistent-object))
          (values +persistent-object-code+ #t
                  (lambda (instance context)
                    (bind ((class (class-of instance)))
                      (serializer::serialize-symbol (class-name class) context)
                      (funcall persistent-object-serializer instance context))))
          (values code has-identity writer-function)))))

(def (function o) make-export-deserializer-mapper (persistent-object-deserializer)
  (lambda (code context)
    (if (eq code +persistent-object-code+)
        (lambda (context &optional referenced)
          (declare (ignore referenced))
          (bind ((class-name (serializer::deserialize-symbol context))
                 (class (find-class class-name :errorp #f))
                 (prototype-or-class-name (or (and class (closer-mop:class-prototype class))
                                              class-name)))
            (funcall persistent-object-deserializer prototype-or-class-name context)))
        (cl-serializer::default-deserializer-mapper code context))))

(def (function e) write-persistent-object-slot-values (instance context &key exclude-slots)
  (bind ((class (class-of instance))
         (slots (collect-if (lambda (slot)
                              (and (persistent-slot-p slot)
                                   (not (eq (closer-mop:slot-definition-allocation slot) :class))
                                   (not (member (closer-mop:slot-definition-name slot) exclude-slots))))
                            (class-slots class))))
    (write-persistent-object-oid (oid-of instance) context)
    (serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
      (if (closer-mop:slot-boundp-using-class class instance slot)
          (serializer::serialize-element (closer-mop:slot-value-using-class class instance slot) context)
          (serializer::write-unsigned-byte-8 serializer::+unbound-slot-code+ context)))))

(def (function e) read-persistent-object-slot-values (prototype-or-class-name context &optional (persistp #t))
  (bind ((class (etypecase prototype-or-class-name
                  (symbol (find-class prototype-or-class-name))
                  (persistent-object (class-of prototype-or-class-name))))
         (oid (make-new-oid (class-name class)))
         (instance (allocate-instance class))
         (old-oid (read-persistent-object-oid context)))
    (initialize-revived-instance instance :persistent #f :oid oid)
    (serializer::announce-identity instance context)
    (iter (repeat (the fixnum (serializer::read-variable-length-positive-integer context)))
          (for slot-name = (serializer::deserialize-symbol context))
          (if (eq serializer::+unbound-slot-code+ (serializer::read-unsigned-byte-8 context))
              (slot-makunbound instance slot-name)
              (setf (slot-value instance slot-name)
                    (progn
                      (serializer::unread-unsigned-byte-8 context)
                      (serializer::deserialize-element context)))))
    (when persistp
      (make-persistent instance))
    (values instance old-oid)))

(def (function e) dump-persistent-object-slot-values (prototype-or-class-name context)
  (bind ((class (etypecase prototype-or-class-name
                  (symbol (find-class prototype-or-class-name))
                  (persistent-object (class-of prototype-or-class-name))))
         (class-name (class-name class))
         (oid (read-persistent-object-oid context))
         (instance (list :object class-name oid)))
    (serializer::announce-identity (list :reference class-name oid) context)
    (iter (repeat (the fixnum (serializer::read-variable-length-positive-integer context)))
          (for slot-name = (serializer::deserialize-symbol context))
          (nconcf instance
                  (list (intern (symbol-name slot-name) :keyword)
                        (if (eq serializer::+unbound-slot-code+ (serializer::read-unsigned-byte-8 context))
                            :unbound
                            (progn
                              (serializer::unread-unsigned-byte-8 context)
                              (serializer::deserialize-element context))))))
    instance))

;;;;;;
;;; XML

(def method export-persistent-instances (thing (format (eql :xml)) stream &key &allow-other-keys)
  (bind ((*xml-stream* stream)
         (seen-set (make-hash-table)))
    (labels ((recurse (thing)
               (if (gethash thing seen-set)
                   <reference (:oid ,(oid-of thing))>
                   (etypecase thing
                     (list <list ,(mapc #'recurse thing)>)
                     (persistent-object
                      (bind ((instance thing)
                             (class (class-of instance))
                             (class-name (string-downcase (class-name class)))
                             (slots (remove-if (of-type 'persistent-effective-slot-definition-d) (persistent-effective-slots-of class))))
                        (setf (gethash thing seen-set) #t)
                        <,class-name (:oid ,(oid-of instance)
                                           ,@(iter (for slot :in slots)
                                                   (when (and (not (typep slot 'persistent-association-end-effective-slot-definition))
                                                              (not (persistent-class-type-p* (canonical-type-of slot)))
                                                              (closer-mop:slot-boundp-using-class class instance slot))
                                                     (bind ((slot-name (string-downcase (symbol-name (closer-mop:slot-definition-name slot))))
                                                            (value (closer-mop:slot-value-using-class class instance slot))
                                                            (slot-value (etypecase value
                                                                          ((member #f #t) (if value "#t" "#f"))
                                                                          (string value)
                                                                          (symbol (canonical-symbol-name value))
                                                                          (number (princ-to-string value))
                                                                          (timestamp (format-timestring nil value
                                                                                                        :format '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
                                                                                                                  (:hour 2) #\: (:min 2) #\: (:sec 2) #\.
                                                                                                                  (:usec 6) :gmt-offset)
                                                                                                        :timezone +utc-zone+)))))
                                                       (collect (cl-quasi-quote-xml:make-xml-attribute slot-name slot-value))))))
                                     ,(iter (for slot :in slots)
                                            (when (and (typep slot 'persistent-association-end-effective-slot-definition)
                                                       (closer-mop:slot-boundp-using-class class instance slot))
                                              (bind ((slot-name (string-downcase (symbol-name (closer-mop:slot-definition-name slot))))
                                                     (value (closer-mop:slot-value-using-class class instance slot)))
                                                (if value
                                                    <,slot-name ,(ecase (cardinality-kind-of slot)
                                                                        (:1 (recurse value))
                                                                        (:n (map nil #'recurse value)))>
                                                    <,slot-name>))))>))))))
      (recurse thing))))

(def method import-persistent-instances ((format (eql :xml)) stream &key &allow-other-keys)
  (not-yet-implemented))
