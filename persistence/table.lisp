(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS model classes

;; TODO: use sql-table when available
(defcclass* table ()
  ((name
    :type symbol
    :documentation "The name of the RDBMS table.")
   (columns
    (compute-as nil)
    :type (list sql-column)
    :documentation "The list of RDBMS columns of this table. This list uses the sql column type of cl-rdbms."))
  (:documentation "An RDBMS table with some related RDBMS definitions. The actual table will be created in the database when export-to-rdbms is called on it."))

(defcclass* column (sql-column)
  ((index
    (compute-as nil)
    :type (or null sql-index)
    :documentation "An RDBMS index on this column."))
  (:documentation "An RDBMS column with some related RDBMS specific definitions."))

(defcclass* class-primary-table (table)
  ((oid-columns
    (compute-as (list (id-column-of -self-) (class-name-column-of -self-)))
    :type (list sql-column)
    :documentation "The list of RDBMS columns corresponding to the oid of this table.")
   (id-column
    (compute-as (find +id-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of corresponding oid slot.")
   (class-name-column
    (compute-as (find +class-name-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column
    :documentation "The RDBMS column of corresponding oid slot."))
  (:documentation "This is a special table related to a persistent class."))

(defprint-object (self table)
  (princ (name-of self)))

(defprint-object (self column)
  (princ (rdbms::name-of self)))

;;;;;;;;;;;;;
;;; Constants

(defconstant +oid-id-bit-size+ 64
  "Length of the life time unique identifier numbers in bits.")

(defvar +oid-id-sql-type+
  (make-instance 'sql-integer-type :bit-size +oid-id-bit-size+)
  "The RDBMS type for the oid's id slot.")

(defconstant +oid-class-name-maximum-length+ 64
  "Maximum length of class names.")

(defvar +oid-class-name-sql-type+
  (make-instance 'sql-character-varying-type :size +oid-class-name-maximum-length+)
  "The RDBMS type for the oid's class-name slot")

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((table table))
  "Updates the RDBMS table definition according to the current state of the given table. This might add, alter or drop existing columns, but all destructive changes are required to signal a continuable condition."
  (update-table (name-of table) (columns-of table))
  (mapc #L(awhen (index-of !1)
            (update-index (rdbms::name-of it) (name-of table) (list !1)))
        (columns-of table)))

;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun rdbms-name-for (name)
  "Returns a name which does not conflict with RDBMS keywords."
  ;; TODO: this name mapping is not injective (different lisp names are mapped to the same rdbms name)
  (let ((name-as-string (strcat "_" (regex-replace-all "\\*|-|/" (symbol-name name) "_"))))
    (if (symbol-package name)
        (intern name-as-string (symbol-package name))
        (make-symbol name-as-string))))
