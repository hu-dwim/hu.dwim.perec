(in-package :cl-perec)

;;;; Functions called from compiled queries.

;;;
;;; Lisp implementation of some SQL funtion
;;;
(defun like (str pattern)
  "Matches STR with PATTERN. In the pattern _ and % wildcards can be used"
  (flet ((like-pattern->regex (pattern)
           (setf pattern (regex-replace-all "([.*+?(){}|^$])" pattern "\\\\\\1"))
           (setf pattern (regex-replace-all "(?<!\\\\)_" pattern "."))
           (setf pattern (regex-replace-all "(?<!\\\\)%" pattern ".*"))))
    (if (scan (like-pattern->regex pattern) str) #t #f)))

(defun sum (seq)
  "Returns the sum of non NIL elements of SEQ."
  (iter (for val in-sequence seq)
        (sum (or val 0))))

(defun avg (seq)
  "Returns the average of non NIL elements of SEQ."
  (iter (for val in-sequence seq)
        (sum (or val 0) into sum)
        (counting val into count)
        (finally (return (if (> count 0) (/ sum count) 0)))))

;;;
;;; Caching
;;;
(defun cache-object-with-prefetched-properties (row start prefetched-slots)
  "Caches the objects whose oid and slots are contained by ROW starting at START."
  (bind ((oid-width (length +oid-column-names+))
         (oid (subseq row start (+ start oid-width)))
         (rdbms-values
          (iter (for slot in prefetched-slots)
                (for width = (column-count-of slot))
                (for index initially (+ start oid-width) then (+ index width))
                (collect (subseq row index (+ index width))))))
    (cache-object* oid prefetched-slots rdbms-values)))

(defun cache-object* (oid slots rdbms-values)
  "Caches the objects whose oid and slots are contained by ROW starting at START."
  ;; PORT: remove it
  (declare (ignore slots rdbms-values))
  (bind ((object (cache-object oid)))
    ;; PORT: port it
    #+nil(mapc #L(setf (cached-slot-value object (name-of !1))
                  (slot-value-from-rdbms-values object !1 !2))
          slots rdbms-values)
    object))

(defun column-count-of (slot)
  (length (columns-of slot)))

(defun invalidate-persistent-flag-of-cached-objects (class)
  "Sets the persistent slot to unbound for instances of class in the transaction cache."
  (maphash
   (lambda (oid object)
     (declare (ignore oid))
     (when (typep object class)
       (slot-makunbound object 'persistent)))
   (objects-of (current-object-cache))))

;;;
;;; Conversion between lisp and sql values
;;;

(defgeneric value->sql-literal (value type)
  (:method (value type)
           (error "Don't know how to map ~A to an SQL literal.~%" value))

  (:method ((value persistent-object) type)
           (sql-literal :value (id-of value)))

  (:method ((value string) type)
           (sql-literal :value value))

  (:method ((value number) type)
           (sql-literal :value value))

  (:method ((value list) type)
           (sql-literal :value (mapcar #L(value->sql-literal !1 nil) value)))

  (:method ((value (eql nil)) type)
           (sql-literal :value nil))

  (:method ((value (eql t)) type)
           (sql-literal :value t))

  (:method ((value symbol) type)
           (sql-literal :value (canonical-symbol-name value)))

  (:method ((value local-time) type)
           (sql-literal :value (local-time->string value)))

  ;; PORT:
  #+nil(:method ((value symbol) (type enumerated-type))
           (sql-literal :value (enum->integer value type)))

  )

;;; TODO: eliminate duplication with transformer.lisp

(defun local-time->string (value)
  (format-timestring value :date-time-separator #\Space :use-zulu-p #f))

;; PORT:
#+nil
(defun enum->integer (value enum-type)
  (loop for i from 0
        for member in (enumeration-members-of enum-type)
        when (eq value member)
        do (return i)))


