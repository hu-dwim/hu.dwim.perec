(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS model classes

;; TODO: use sql-table when available from cl-rdbms
(defcclass* table (exportable)
  ((name
    :type symbol
    :documentation "The name of the RDBMS table.")
   (columns
    (compute-as nil)
    :type (list sql-column)
    :documentation "The list of RDBMS columns of this table. This list uses the sql column type of cl-rdbms."))
  (:documentation "An RDBMS table with some related RDBMS definitions. The actual table will be created in the database when export-to-rdbms is called on it."))

(defcclass* view (exportable)
  ((name
    :type symbol
    :documentation "The name of the RDBMS view.")
   (columns
    :type list
    :documentation "The column names of the view.")
   (query
    :type sql-query-expression
    :documentation "The SQL create view statement.")))

(defcclass* column (sql-column)
  ((index
    (compute-as nil)
    :type (or null sql-index)
    :documentation "An RDBMS index on this column."))
  (:documentation "An RDBMS column with some related RDBMS specific definitions."))

(def print-object table
  (princ (name-of -self-)))

(def print-object column
  (princ (rdbms::name-of -self-)))

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((table table))
  "Updates the RDBMS table definition according to the current state of the given table. This might add, alter or drop existing columns, but all destructive changes are required to signal a continuable condition."
  (update-table (name-of table) (columns-of table))
  (mapc #L(awhen (index-of !1)
            (update-index (rdbms::name-of it) (name-of table) (list !1) :unique (rdbms::unique-p it)))
        (columns-of table)))

(defmethod export-to-rdbms ((view view))
  (update-view (name-of view) (columns-of view) (query-of view)))
