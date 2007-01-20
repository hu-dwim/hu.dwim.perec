(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS model classes

(defclass* table ()
  ((name
    (compute-as nil)
    :type symbol
    :documentation "The name of the RDBMS table.")
   (columns
    (compute-as nil)
    :type (list column)
    :documentation "The list of RDBMS columns of this table.")
   (triggers
    (compute-as nil)
    :type (list trigger)
    :documentation "The RDBMS triggers defined on this table."))
  (:documentation "This is an RDBMS table with some related RDBMS definitions. The actual table will be created in the database when export-table is called on it.")
  (:metaclass computed-class))

(defclass* column ()
  ((name
    (compute-as nil)
    :type symbol
    :documentation "The name of the RDBMS column.")
   (table
    (compute-as nil)
     :type table
     :documentation "The RDBMS table of which this column is part of.")
   (column-type
    (compute-as nil)
    :type symbol
    :documentation "The RDBMS type of the column. This is different from the lisp type of the corresponding slot.")
   (constraints
    nil
    :type list
    :documentation "Any additional RDBMS constraint that will be added to this column in the database.")
   (sql-column
    (compute-as nil)
    :type sql-column
    :documentation "SQL AST node"))
  (:documentation "An RDBMS column of a table.")
  (:metaclass computed-class))

(defclass* class-primary-table (table)
  ((oid-columns
    (compute-as nil)
    :type (list column)
    :documentation "The list of RDBMS columns corresponding to the oid of this table.")
   (id-column
    (compute-as nil)
    :type column)
   (class-name-column
    (compute-as nil)
    :type column))
  (:metaclass computed-class))

(defclass* trigger ()
  ((table
    (compute-as nil)
    :type table
    :documentation "The RDBMS table on which this trigger is defined.")
   (command
    :type string
    :documentation "The SQL command that defines this RDBMS trigger."))
  (:documentation "An RDBMS trigger")
  (:metaclass computed-class))

(defconstant +oid-id-bit-size+ 64)

(defvar +oid-id-sql-type+
  (make-instance 'sql-integer-type :bit-size +oid-id-bit-size+))

(defconstant +oid-class-name-maximum-length+ 64)

(defvar +oid-class-name-sql-type+
  (make-instance 'sql-character-varying-type :size +oid-class-name-maximum-length+))
