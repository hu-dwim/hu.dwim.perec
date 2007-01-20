(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS model classes

(defclass* table ()
  ((name
    :type symbol
    :documentation "The name of the RDBMS table.")
   (columns
    (compute-as nil)
    :type (list sql-column)
    :documentation "The list of RDBMS columns of this table."))
  (:documentation "This is an RDBMS table with some related RDBMS definitions. The actual table will be created in the database when export-table is called on it.")
  (:metaclass computed-class))

(defclass* class-primary-table (table)
  ((oid-columns
    (compute-as (list (id-column-of -self-) (class-name-column-of -self-)))
    :type (list sql-column)
    :documentation "The list of RDBMS columns corresponding to the oid of this table.")
   (id-column
    (compute-as (find +id-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column)
   (class-name-column
    (compute-as (find +class-name-column-name+ (columns-of -self-) :key 'cl-rdbms::name-of))
    :type sql-column))
  (:metaclass computed-class))

(defconstant +oid-id-bit-size+ 64)

(defvar +oid-id-sql-type+
  (make-instance 'sql-integer-type :bit-size +oid-id-bit-size+))

(defconstant +oid-class-name-maximum-length+ 64)

(defvar +oid-class-name-sql-type+
  (make-instance 'sql-character-varying-type :size +oid-class-name-maximum-length+))

;;;;;;;;;;;;;;;;;;
;;; Helper methods

(defun rdbms-name-for (name)
  ;; TODO: this name replacement is not injective (different lisp names are mapped to the same rdbms name)
  (let ((name-as-string (strcat "_" (regex-replace-all "\\*|-|/" (symbol-name name) "_"))))
    (if (symbol-package name)
        (intern name-as-string (symbol-package name))
        (make-symbol name-as-string))))
