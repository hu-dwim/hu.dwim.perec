(in-package :cl-perec)

;;;;;;;;;;;;;;;;;;;;;;;
;;; RDBMS model classes

;; TODO: use sql-table when available
(defcclass* table (exportable)
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

(defprint-object (self table)
  (princ (name-of self)))

(defprint-object (self column)
  (princ (rdbms::name-of self)))

;;;;;;;;;;;;;
;;; Constants

(defun equal-type-p (type-1 type-2)
  (rdbms::equal-type-p type-1 type-2 nil))

(define-constant +oid-id-sql-type+ (sql-integer-type :bit-size +oid-id-bit-size+)
  :test equal-type-p
  :documentation "The RDBMS type for the oid's id slot.")

(define-constant +oid-class-id-sql-type+ (sql-integer-type :bit-size +oid-class-id-bit-size+)
  :test equal-type-p
  :documentation "The RDBMS type for the oid's class-id slot")

(define-constant +oid-class-name-sql-type+ (sql-character-varying-type :size +oid-class-name-character-size+)
  :test equal-type-p
  :documentation "The RDBMS type for the oid's class-name slot")

;;;;;;;;;;
;;; Export

(defmethod export-to-rdbms ((table table))
  "Updates the RDBMS table definition according to the current state of the given table. This might add, alter or drop existing columns, but all destructive changes are required to signal a continuable condition."
  (update-table (name-of table) (columns-of table))
  (mapc #L(awhen (index-of !1)
            (update-index (rdbms::name-of it) (name-of table) (list !1)))
        (columns-of table)))

;;;;;;;;;;;
;;; Utility

;; TODO get rid of this, use string names
(defun rdbms-name-for (name &optional thing)
  (let ((name-as-string (rdbms:rdbms-name-for name thing)))
    (aif (symbol-package name)
         (intern name-as-string (if (eq it #.(find-package :common-lisp))
                                    (find-package :cl-rdbms)
                                    it))
         (make-symbol name-as-string))))
