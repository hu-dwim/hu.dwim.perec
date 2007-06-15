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
     :class-id   - _ID and _CLASS_ID columns where id stores instance if
     :merge      - _ID column where id stores instance id and class id merged into one integer class id being the lower bits")

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

(defconstant +oid-maximum-class-id+ (1- (expt 2 +oid-class-id-bit-size+))
  "Maximum class id available.")

(defconstant +oid-class-name-character-size+ 128
  "Maximum length of class names.")

(defconstant +oid-id-bit-size+ 64
  "Size of the id in bits.")

(defconstant +oid-instance-id-sequence-name+ '_instance_id
  "The name of the instance id sequence in the relational database used to generate life time unique identifiers for all persistent instances.")

(defconstant +oid-class-id-sequence-name+ '_class_id
  "The name of the class id sequence in the relational database used to generate life to unique class identifiers for all persistent classes.")

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

(defparameter *oid-instance-id-sequence-exists* #f
  "Tells if the instance id sequence exists in the relational database or not. This flag is initially false but will be set to true as soon as the sequence is created or first time seen in the database when a persistent instance is restored or stored.")

(defparameter *oid-class-id-sequence-exists* #f
  "Tells if the class id sequence exists in the relation database or not. This flag is initially false but will be set to true as soon as the sequence is created or first time seen in the database when a persistent class is exported.")

(defparameter *oid-class-id->class-name-map* (make-hash-table)
  "This map is used to cache class names by class ids. It gets filled when ensure-class is called for the first time and kept up to date.")

(defparameter *oid-class-name->class-id-map* (make-hash-table)
  "This map is used to cache class ids by class names. It gets filled when ensure-class is called for the first time and kept up to date.")

(defun class-id->class-name (class-id)
  (gethash class-id *oid-class-id->class-name-map*))

(defun (setf class-id->class-name) (class-name class-id)
  (debug-only
    (assert (and class-id class-name (symbolp class-name) (integerp class-id))))
  (setf (gethash class-id *oid-class-id->class-name-map*) class-name))

(defun class-name->class-id (class-name)
  (gethash class-name *oid-class-name->class-id-map*))

(defun (setf class-name->class-id) (class-id class-name)
  (debug-only
    (assert (and class-id class-name (integerp class-id) (symbolp class-name))))
  (setf (gethash class-name *oid-class-name->class-id-map*) class-id))

(defun make-class-oid (class-name)
  "Creates a fresh and unique oid which was never used before in the relational database."
  (or *oid-instance-id-sequence-exists* (ensure-instance-id-sequence))
  (bind ((instance-id (next-instance-id))
         (class-id (class-name->class-id class-name))
         (id (oid-mode-ecase
               (:class-name instance-id)
               (:class-id instance-id)
               (:merge (logior (ash instance-id +oid-class-id-bit-size+) class-id)))))
    (make-oid :id id
              :instance-id instance-id
              :class-id class-id
              :class-name class-name)))

(defun ensure-instance-id-sequence ()
  "Makes sure the instance id sequence exists in the database."
  (unless (sequence-exists-p +oid-instance-id-sequence-name+)
    (create-sequence +oid-instance-id-sequence-name+))
  (setf *oid-instance-id-sequence-exists* #t))

(defun next-instance-id ()
  (aprog1 (sequence-next +oid-instance-id-sequence-name+)
    (assert (<= (integer-length it) +oid-instance-id-bit-size+))))

(defun next-class-id ()
  (aprog1 (sequence-next +oid-class-id-sequence-name+)
    (assert (< it +oid-maximum-class-id+))))

(defun ensure-class-id-sequence ()
  "Makes sure the class id sequence exists in the database."
  (unless (sequence-exists-p +oid-class-id-sequence-name+)
    (create-sequence +oid-class-id-sequence-name+))
  (setf *oid-class-id-sequence-exists* #t))

(defun oid->rdbms-values (oid)
  "Returns a sufficient list representation for relational database operations of the instance's oid in the order of the corresponding RDBMS columns."
  (bind ((id (oid-id oid)))
    (oid-mode-ecase
      (:class-name (list id (oid-class-name oid)))
      (:class-id (list id (oid-class-id oid)))
      (:merge (list id)))))

(defun rdbms-values->oid (rdbms-values)
  (oid-mode-ecase
    (:class-name
     (bind ((instance-id (first rdbms-values)))
       (make-oid :id instance-id
                 :instance-id (first rdbms-values)
                 :class-name (symbol-from-canonical-name (second rdbms-values)))))
    (:class-id
     (bind ((instance-id (first rdbms-values))
            (class-id (second rdbms-values)))
       (make-oid :id instance-id
                 :instance-id instance-id 
                 :class-id class-id
                 :class-name (class-id->class-name class-id))))
    (:merge
     (bind ((id (first rdbms-values))
            (instance-id (ldb (byte +oid-instance-id-bit-size+ +oid-class-id-bit-size+) id))
            (class-id (ldb (byte +oid-class-id-bit-size+ 0) id)))
       (make-oid :id id
                 :instance-id instance-id
                 :class-id class-id
                 :class-name (class-id->class-name class-id))))))
