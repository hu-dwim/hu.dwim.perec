;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defstruct oid
  "The oid holds sufficient information to access the same persistent instance at any time in the database unless it has been deleted. The instance id of an instance is constant and unique for the whole lifetime of the instance and cannot be changed. It is assigned when the persistent instance is first time stored in the database."
  (id ;; stored in the id columns; either the instance id or the merged value of the instance and the class id
   nil
   :type (or null integer))
  (instance-id ;; a life time constant and unique identifier
   nil
   :type (or null integer))
  (class-id ;; a constant and unique class identifier kept between sessions
   nil
   :type (or null fixnum))
  (class-name ;; the name of the class
   nil
   :type (or null symbol)))

(defconstant +oid-mode+ :merge
  "Specifies the mode oids are stored in the database. 
     :class-name - _ID and _CLASS_NAME columns where id stores instance id
     :class-id   - _ID and _CLASS_ID columns where id stores instance id
     :merge      - _ID column where id stores instance id and class id merged into one integer where class id are the lower bits")

(defmacro oid-mode-ecase (&body cases)
  `(locally (declare #+sbcl(sb-ext:muffle-conditions sb-ext:compiler-note))
    (ecase +oid-mode+
      ,@cases)))

(defconstant +oid-instance-id-bit-size+ (if (eq :merge +oid-mode+)
                                            48
                                            64)
  "Size of the life time unique identifier instance id in bits.")

(defconstant +oid-maximum-instance-id+ (1- (expt 2 +oid-instance-id-bit-size+))
  "Maximum instance id available.")

(defconstant +oid-class-id-bit-size+ 16
  "Size of the life time unique identifier class id in bits.")

(defconstant +oid-maximum-class-id+ (expt 2 +oid-class-id-bit-size+)
  "Maximum class id available.")

(defconstant +oid-class-name-character-size+ 128
  "Maximum length of class names.")

(defconstant +oid-id-bit-size+ 64
  "Size of the id in bits.")

(defconstant +oid-instance-id-sequence-name+ '_instance_id
  "The name of the instance id sequence in the relational database used to generate life time unique identifiers for all persistent instances.")

(defconstant +oid-id-column-name+ '_id
  "The RDBMS table column name for the oid's id slot.")

(defconstant +oid-class-id-column-name+ '_class_id
  "The RDBMS table column name for the oid's class id slot.")

(defconstant +oid-class-name-column-name+ '_class_name
  "The RDBMS table column name for the oid's class name slot.")

(define-constant +oid-column-names+ (oid-mode-ecase
                                      (:class-name (list +oid-id-column-name+ +oid-class-name-column-name+))
                                      (:class-id (list +oid-id-column-name+ +oid-class-id-column-name+))
                                      (:merge (list +oid-id-column-name+)))
  :test equal
  :documentation "All RDBMS table column names for an oid. The order of columns does matter.")

(defconstant +oid-column-count+ (length +oid-column-names+)
  "The number of oid columns")

(defparameter *oid-instance-id-sequence-exists* #f
  "Tells if the instance id sequence exists in the relational database or not. This flag is initially false but will be set to true as soon as the sequence is created or first time seen in the database when a persistent instance is restored or stored.")

(defparameter *oid-class-id->class-name-map* (make-hash-table)
  "This map is used to cache class names by class ids. It gets filled when ensure-class is called for the first time and kept up to date.")

(def (function o) class-id->class-name (class-id)
  (aprog1
      (gethash class-id *oid-class-id->class-name-map*)
    (assert it nil "Could not find the class name for the class id ~A, probably the class has not yet been exported." class-id)))

(def (function o) (setf class-id->class-name) (class-name class-id)
  (assert (and class-id class-name (symbolp class-name) (integerp class-id)))
  (awhen (gethash class-id *oid-class-id->class-name-map*)
    (unless (eq it class-name)
      (error "Two different class names have the same class id ~A ~A" it class-name)))
  (setf (gethash class-id *oid-class-id->class-name-map*) class-name))

(def (function io) class-name->class-id (class-name)
  (mod (sxhash (symbol-name class-name))
       +oid-maximum-class-id+))

(def (function io) class-id-and-instance-id->id (class-id instance-id)
  (oid-mode-ecase
    (:class-name instance-id)
    (:class-id instance-id)
    (:merge (logior (ash instance-id +oid-class-id-bit-size+) class-id))))

(def (function o) make-class-oid (class-name)
  "Creates a fresh and unique oid which was never used before in the relational database."
  (or *oid-instance-id-sequence-exists* (ensure-instance-id-sequence))
  (bind ((class-id (class-name->class-id class-name))
         (instance-id (next-instance-id))
         (id (class-id-and-instance-id->id class-id instance-id)))
    (make-oid :id id
              :instance-id instance-id
              :class-id class-id
              :class-name class-name)))

(def (function io) revive-oid (class-id instance-id)
  (make-oid :id (class-id-and-instance-id->id class-id instance-id)
            :class-id class-id
            :instance-id instance-id
            :class-name (class-id->class-name class-id)))

(def (function o) ensure-instance-id-sequence ()
  "Makes sure the instance id sequence exists in the database."
  (unless (sequence-exists-p +oid-instance-id-sequence-name+)
    (create-sequence +oid-instance-id-sequence-name+))
  (setf *oid-instance-id-sequence-exists* #t))

(def (function o) next-instance-id ()
  (aprog1 (sequence-next +oid-instance-id-sequence-name+)
    (unless (<= (integer-length it) +oid-instance-id-bit-size+)
      (error "Instance id sequence reached its maximum value ~A" +oid-maximum-instance-id+))))

(def (function io) oid->rdbms-values (oid)
  (aprog1 (make-array +oid-column-count+)
    (oid->rdbms-values* oid it 0)))

(def (function io) oid->rdbms-values* (oid rdbms-values index)
  "Returns a sufficient list representation for relational database operations of the instance's oid in the order of the corresponding RDBMS columns."
  (bind ((id (oid-id oid)))
    (setf (elt rdbms-values index) id)
    (oid-mode-ecase
      (:class-name
       (setf (elt rdbms-values (1+ index)) (oid-class-name oid)))
      (:class-id
       (setf (elt rdbms-values (1+ index)) (oid-class-id oid)))
      (:merge))))

(def (function io) rdbms-values->oid (rdbms-values)
  (rdbms-values->oid* rdbms-values 0))

(def (function io) rdbms-values->oid* (rdbms-values index)
  (bind ((id (elt rdbms-values index))
         instance-id
         class-id
         class-name)
    (oid-mode-ecase
      (:class-name
       (setf instance-id id)
       (setf class-name (symbol-from-canonical-name (elt rdbms-values (1+ index)))))
      (:class-id
       (setf instance-id id)
       (setf class-id (elt rdbms-values (1+ index)))
       (setf class-name (class-id->class-name class-id)))
      (:merge
       (setf instance-id (ldb (byte +oid-instance-id-bit-size+ +oid-class-id-bit-size+) id))
       (setf class-id (ldb (byte +oid-class-id-bit-size+ 0) id))
       (setf class-name (class-id->class-name class-id))))
    (debug-only
      (assert (and id instance-id class-name)))
    (make-oid :id id
              :instance-id instance-id
              :class-id class-id
              :class-name class-name)))
