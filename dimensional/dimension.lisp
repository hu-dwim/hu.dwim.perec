;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Dimension

(def class* abstract-dimension ()
  ((name
    :type symbol)
   (dependent-object-name
    :type symbol)
   (type
    :accessor the-type-of)))

(def class* dimension (abstract-dimension)
  ((coordinate-name
    :type symbol)
   (default-coordinate
     nil
    :type function)))

(def class* ordering-dimension (abstract-dimension)
  ((begin-coordinate-name
    :type symbol)
   (end-coordinate-name
    :type symbol)
   (default-begin-coordinate
     nil
    :type function)
   (default-end-coordinate
     nil
    :type function)
   (minimum-coordinate
    nil)
   (maximum-coordinate
    nil)))

(def class* inheriting-dimension (ordering-dimension)
  ((direction
    :type (member :ascending :descending))))

(def print-object abstract-dimension ()
  (princ (name-of -self-)))

(def special-variable *dimensions* (make-hash-table) "The list of defined dimensions")

(def (function e) find-dimension (name &key (otherwise :error))
  (or (gethash name *dimensions*)
      (handle-otherwise otherwise)))

(def function (setf find-dimension) (dimension name)
  (setf (gethash name *dimensions*) dimension))

;;;;;;
;;; Defining

(def (definer :available-flags "e") dimension (name &key type ordered inherit
                                                    (default-coordinate nil default-coordinate?)
                                                    (default-begin-coordinate nil default-begin-coordinate?)
                                                    (default-end-coordinate nil default-end-coordinate?)
                                                    (minimum-coordinate nil minimum-coordinate?)
                                                    (maximum-coordinate nil maximum-coordinate?))
  (when inherit
    (setf ordered #t))
  (bind ((dimension-class-name (cond (inherit 'inheriting-dimension)
                                     (ordered 'ordering-dimension)
                                     (t 'dimension)))
         (dimension-variable-name (format-symbol *package* "*~A-DIMENSION*" name))
         (dependent-object-name (format-symbol *package* "~A-DEPENDENT-OBJECT" name))
         (dependent-instances-name (format-symbol *package* "~A-DEPENDENT-INSTANCES" name))
         (begin-variable-name (format-symbol *package* "~A-BEGIN" name))
         (begin-special-name (format-symbol *package* "*~A*" begin-variable-name))
         (end-variable-name (format-symbol *package* "~A-END" name))
         (end-special-name (format-symbol *package* "*~A*" end-variable-name))
         (coordinate-name (format-symbol *package* "*~A*" name))
         (with-macro-name (format-symbol *package* "WITH-~A" name))
         (with-range-macro-name (format-symbol *package* "WITH-~A-RANGE" name))
         (with-range-from-macro-name (format-symbol *package* "WITH-~A-FROM" name))
         (with-range-to-macro-name (format-symbol *package* "WITH-~A-TO" name))
         (call-with-fn-name (format-symbol *package* "CALL-~A" with-macro-name))
         (call-with-range-fn-name (format-symbol *package* "CALL-~A" with-range-macro-name))
         (dimension-args (if ordered
                             `(:begin-coordinate-name ',begin-special-name
                                                      :end-coordinate-name ',end-special-name
                                                      ,@(when default-begin-coordinate?
                                                              `(:default-begin-coordinate (lambda () ,default-begin-coordinate)))
                                                      ,@(when default-end-coordinate?
                                                              `(:default-end-coordinate (lambda () ,default-end-coordinate)))
                                                      ,@(when minimum-coordinate?
                                                              `(:minimum-coordinate ,minimum-coordinate))
                                                      ,@(when maximum-coordinate?
                                                              `(:maximum-coordinate ,maximum-coordinate))
                                                      ,@(when inherit (list :direction inherit)))
                             `(:coordinate-name ',coordinate-name
                                                ,@(when default-coordinate?
                                                        `(:default-coordinate (lambda () ,default-coordinate))))))
         (slots (unless (persistent-class-name-p type)
                  (if (and ordered (not inherit))
                      `((,begin-variable-name :type ,type)
                        (,end-variable-name :type ,type))
                      `((,name :type ,type))))))
    `(progn
       ,(when (getf -options- :export)
              `(export ',name))
       (def (special-variable e) ,dimension-variable-name
           (make-instance ',dimension-class-name
                          :name ',name
                          :dependent-object-name ',dependent-object-name
                          :type ',type
                          ,@dimension-args))
       (setf (find-dimension ',name) ,dimension-variable-name)
       (defpclass* ,dependent-object-name ()
         ,slots
         (:abstract #t))
       ,@(when (persistent-class-name-p type)
               `((defassociation*
                   ((:class ,dependent-object-name :slot ,name :type ,type)
                    (:class ,type :slot ,dependent-instances-name :type (set ,dependent-object-name))))))
       ,@(if ordered
             `((def (macro e) ,with-macro-name (,name &body forms)
                 `(,',call-with-range-fn-name
                   ,(coerce-to-coordinate-begin ,name ',type)
                   ,(coerce-to-coordinate-end ,name ',type)
                   (lambda () ,@forms)))
               (def (special-variable e) ,begin-special-name)
               (def (special-variable e) ,end-special-name)
               (def (symbol-macro e) ,coordinate-name
                   (coordinate (find-dimension ',name)))
               (def (function e) ,call-with-fn-name (,name thunk)
                 (bind ((,begin-special-name ,name)
                        (,end-special-name ,name))
                   (funcall thunk)))
               (def macro ,with-range-macro-name (,begin-variable-name ,end-variable-name &body forms)
                 `(,',call-with-range-fn-name
                   ,(coerce-to-coordinate-begin ,begin-variable-name ',type)
                   ,(coerce-to-coordinate-end ,end-variable-name ',type)
                   (lambda () ,@forms)))
               ,@(when maximum-coordinate?
                       `((def (macro e) ,with-range-from-macro-name (begin &body forms)
                           `(,',with-range-macro-name ,begin ,,maximum-coordinate ,@forms))))
               ,@(when minimum-coordinate?
                       `((def (macro e) ,with-range-to-macro-name (end &body forms)
                           `(,',with-range-macro-name ,,minimum-coordinate ,end ,@forms))))
               (def function ,call-with-range-fn-name (,begin-variable-name ,end-variable-name thunk)
                 (bind ((,begin-special-name ,begin-variable-name)
                        (,end-special-name ,end-variable-name))
                   (funcall thunk))))
             `((def (macro e) ,with-macro-name (,name &body forms)
                 `(,',call-with-fn-name
                   ,(coerce-to-coordinate ,name ',type)
                   (lambda () ,@forms)))
               (def (special-variable e) ,coordinate-name)
               (def (function e) ,call-with-fn-name (,name thunk)
                 (bind ((,coordinate-name (ensure-list ,name)))
                   (funcall thunk))))))))

(def function coerce-to-coordinate (form type)
  (case type
    (timestamp (if (stringp form)
                   (parse-timestring form)
                   form))
    (t form)))

(def function coerce-to-coordinate-begin (form type)
  (case type
    (timestamp (if (stringp form)
                   (first-moment-for-partial-timestamp form)
                   form))
    (t form)))

(def function coerce-to-coordinate-end (form type)
  (case type
    (timestamp (if (stringp form)
                   (last-moment-for-partial-timestamp form)
                   form))
    (t form)))

;;;;;;
;;; Functional

(def (function e) lookup-dimension (dimension)
  (if (symbolp dimension)
      (find-dimension dimension)
      dimension))

(def (generic e) coordinate (dimension)
  (:method ((dimension symbol))
    (coordinate (lookup-dimension dimension)))

  (:method ((dimension dimension))
    (lookup-coordinate dimension #'coordinate-name-of #'default-coordinate-of))

  (:method ((dimension ordering-dimension))
    (bind ((begin-coordinate (begin-coordinate dimension))
           (end-coordinate (end-coordinate dimension)))
      (unless (equal begin-coordinate end-coordinate)
        (error "The ~A is currently bound to a coordinate range" dimension))
      begin-coordinate)))

(def (function e) begin-coordinate (dimension)
  (lookup-coordinate dimension #'begin-coordinate-name-of #'default-begin-coordinate-of))

(def (function e) end-coordinate (dimension)
  (lookup-coordinate dimension #'end-coordinate-name-of #'default-end-coordinate-of))

(def function lookup-coordinate (dimension name-provider default-provider)
  (bind ((dimension (lookup-dimension dimension))
         (name (funcall name-provider dimension)))
    (if (boundp name)
        (symbol-value name)
        (aif (funcall default-provider dimension)
             (funcall it)
             (error "The coordinate for ~A is unspecified" dimension)))))

;;;;;;
;;; Constants

(def (load-time-constant e) +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  "All timestamps are equal or greater than the beginning of time.")

(def (load-time-constant e) +end-of-time+ (parse-timestring "3000-01-01TZ")
  "All timestamps are equal or less than the end of time.")

;;;;;;
;;; Time dimension

(def (dimension e) time
  :type timestamp
  :inherit :ascending
  :default-begin-coordinate (transaction-timestamp)
  :default-end-coordinate (transaction-timestamp)
  :minimum-coordinate +beginning-of-time+
  :maximum-coordinate +end-of-time+)

;;;;;;
;;; Validity dimension

(def (dimension e) validity
  :type timestamp
  :ordered #t
  :default-begin-coordinate +beginning-of-time+
  :default-end-coordinate +end-of-time+
  :minimum-coordinate +beginning-of-time+
  :maximum-coordinate +end-of-time+)

;;;;;;
;;; Enumerated

(def (dimension e) enumerated
  :type symbol
  :default-coordinate t)
