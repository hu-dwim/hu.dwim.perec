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

(defconstant +oid-id-bit-size+ 64
  "Length of the life time unique identifier numbers in bits.")

(defvar +oid-id-sql-type+
  (sql-integer-type :bit-size +oid-id-bit-size+)
  "The RDBMS type for the oid's id slot.")

(defconstant +oid-class-name-maximum-length+ 128
  "Maximum length of class names.")

(defvar +oid-class-name-sql-type+
  (sql-character-varying-type :size +oid-class-name-maximum-length+)
  "The RDBMS type for the oid's class-name slot")

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

(defun rdbms-name-for (name)
  "Returns a name which does not conflict with RDBMS keywords and fits in the maximum size."
  ;; TODO: this name mapping is not injective (different lisp names are mapped to the same rdbms name)
  (let ((name-as-string (strcat "_" (regex-replace-all "\\*|-|/" (symbol-name name) "_")))
        (max-length 63))
    (when (> (length name-as-string) max-length)
      (setf name-as-string
            (strcat (subseq name-as-string 0 (- max-length 3))
                    (write-to-string (mod (sxhash name-as-string) 1000)))))
    (aif (symbol-package name)
         ;; TODO: find a better place for cl symbols?
         (intern name-as-string (if (eq it #.(find-package :common-lisp))
                                    (find-package :cl-rdbms)
                                    it))
         (make-symbol name-as-string))))
