;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;
;;; Constants

(def (load-time-constant e) +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  "All timestamps are equal or greater than the beginning of time.")

(def (load-time-constant e) +end-of-time+ (parse-timestring "3000-01-01TZ")
  "All timestamps are equal or less than the end of time.")

(def (load-time-constant e) +whole-domain-marker+
  (progn
    (defstruct whole-domain-marker
      "Type of the special value that marks the whole domain of the dimension.")
    (def method make-load-form ((self whole-domain-marker) &optional environment)
      (declare (ignore environment))
      '%%%+whole-domain-marker+)
    (make-whole-domain-marker))
  "The special value that means the whole domain of non-ordering dimensions.")

(def (load-time-constant e) +default-coordinate-marker+
  (progn
    (defstruct default-coordinate-marker
      "Type of the special value that marks the default coordinate of the dimension.")
    (def method make-load-form ((self default-coordinate-marker) &optional environment)
      (declare (ignore environment))
      '%%%+default-coordinate-marker+)
    (make-default-coordinate-marker))
  "The special value that means the default coordinate of dimensions.")

;;;;;;
;;; Dimension

(def class* dimension ()
  ((name
    :type symbol
    :documentation "The name of the dimension.")
   (dependent-object-name
    :type symbol
    :documentation "The name of the persistent class used to hold instances with coordinates in this dimension.")
   (type
    :accessor the-type-of
    :documentation "The type of coordinate system in this the dimension.")
   (coordinate-name
    :type symbol
    :documentation "The name of the special variable that holds the current coordinate for this dimension.")
   (default-coordinate
    nil
    :type (or null function)
    :documentation "A function which computes the default coordinate value for this dimension when the special variable is unbound.")))

(def class* ordering-dimension (dimension)
  ((coordinate-begin-name
    :type symbol)
   (coordinate-end-name
    :type symbol)
   (default-coordinate-begin
     nil
    :type (or null function))
   (default-coordinate-end
     nil
    :type (or null function))
   (minimum-coordinate
    nil
    :type t)
   (maximum-coordinate
    nil
    :type t)))

(def class* inheriting-dimension (ordering-dimension)
  ((direction
    :type (member :ascending :descending)
    :documentation "The direction along which values assigned to different coordinates are inherited.")))

(def print-object dimension ()
  (princ (name-of -self-)))

(def special-variable *dimensions* (make-hash-table) "The list of defined dimensions")

(def (function e) find-dimension (name &key (otherwise :error))
  (or (gethash name *dimensions*)
      (handle-otherwise otherwise)))

(def function (setf find-dimension) (dimension name)
  (setf (gethash name *dimensions*) dimension))

;;;;;;
;;; Defining

(def (definer :available-flags "e") dimension (name &key (type name) ordered inherit external
                                                    (bind-default-coordinate #t)
                                                    (default-coordinate nil default-coordinate?)
                                                    (default-coordinate-begin nil default-coordinate-begin?)
                                                    (default-coordinate-end nil default-coordinate-end?)
                                                    (minimum-coordinate nil minimum-coordinate?)
                                                    (maximum-coordinate nil maximum-coordinate?))
  (when inherit
    (setf ordered #t))
  (bind ((dimension-class-name (cond (inherit 'inheriting-dimension)
                                     (ordered 'ordering-dimension)
                                     (t 'dimension)))
         (dimension-variable-name (format-symbol *package* "*~A-DIMENSION*" name))
         (dependent-object-name (format-symbol *package* "~A-DEPENDENT-OBJECT" name))
         (begin-variable-name (format-symbol *package* "~A-BEGIN" name))
         (begin-special-name (format-symbol *package* "*~A*" begin-variable-name))
         (end-variable-name (format-symbol *package* "~A-END" name))
         (end-special-name (format-symbol *package* "*~A*" end-variable-name))
         (coordinate-name (format-symbol *package* "*~A*" name))
         (with-macro-name (format-symbol *package* "WITH-~A" name))
         (call-with-fn-name (format-symbol *package* "CALL-~A" with-macro-name))
         (dimension-args (if ordered
                             `(:coordinate-name ',coordinate-name
                                                :coordinate-begin-name ',begin-special-name
                                                :coordinate-end-name ',end-special-name
                                                ,@(when (and default-coordinate-begin?
                                                             default-coordinate-end?)
                                                        `(:default-coordinate (lambda () (make-dimension-coordinate-range ,dimension-variable-name ,default-coordinate-begin ,default-coordinate-end))))
                                                ,@(when default-coordinate-begin?
                                                        `(:default-coordinate-begin (lambda () ,default-coordinate-begin)))
                                                ,@(when default-coordinate-end?
                                                        `(:default-coordinate-end (lambda () ,default-coordinate-end)))
                                                ,@(when minimum-coordinate?
                                                        `(:minimum-coordinate ,minimum-coordinate))
                                                ,@(when maximum-coordinate?
                                                        `(:maximum-coordinate ,maximum-coordinate))
                                                ,@(when inherit (list :direction inherit)))
                             `(:coordinate-name ',coordinate-name
                                                ,@(cond
                                                   (default-coordinate?
                                                       `(:default-coordinate (lambda () ,default-coordinate)))
                                                   (t
                                                    `(:default-coordinate (lambda () +whole-domain-marker+)))))))
         (slots (unless (persistent-class-name-p type)
                  (if (and ordered (not inherit))
                      `((,begin-variable-name :type (or unbound ,type))
                        (,end-variable-name :type (or unbound ,type)))
                      `((,name :type (or unbound ,type)))))))
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
         (:abstract #t)
         (:direct-store :push-down))
       ,@(when (persistent-class-name-p type)
               (bind ((dependent-instances-name (format-symbol *package* "~A-DEPENDENT-INSTANCES" name)))
                 `((defassociation*
                     ((:class ,dependent-object-name :slot ,name :type (or null ,type))
                      (:class ,type :slot ,dependent-instances-name :type (set ,dependent-object-name)))))))
       ,@(unless external
                 (if ordered
                     (bind ((with-range-macro-name (format-symbol *package* "WITH-~A-RANGE" name))
                            (with-range-from-macro-name (format-symbol *package* "WITH-~A-FROM" name))
                            (with-range-to-macro-name (format-symbol *package* "WITH-~A-TO" name))
                            (call-with-range-fn-name (format-symbol *package* "CALL-~A" with-range-macro-name)))
                       `((def (macro e) ,with-macro-name (,name &body forms)
                           (if (stringp ,name)
                               `(,',call-with-range-fn-name
                                 ,(coerce-to-coordinate-begin ,name ',type)
                                 ,(coerce-to-coordinate-end ,name ',type)
                                 (lambda () ,@forms))
                               `(,',call-with-fn-name ,,name (lambda () ,@forms))))
                         (def (symbol-macro e) ,begin-special-name
                             (coordinate-begin ,dimension-variable-name))
                         (def (symbol-macro e) ,end-special-name
                             (coordinate-end ,dimension-variable-name))
                         (def (special-variable e) ,coordinate-name
                             ,(if (and bind-default-coordinate
                                       default-coordinate-begin?
                                       default-coordinate-end?)
                                  `(make-dimension-coordinate-range ,dimension-variable-name ,default-coordinate-begin ,default-coordinate-end)
                                  +default-coordinate-marker+))
                         (def (function e) ,call-with-fn-name (,name thunk)
                           (bind ((,coordinate-name (if (coordinate-range-p ,name)
                                                        ,name
                                                        (make-dimension-coordinate-range ,dimension-variable-name ,name))))
                             (funcall thunk)))
                         (def (macro e) ,with-range-macro-name (,begin-variable-name ,end-variable-name &body forms)
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
                         (def (function e) ,call-with-range-fn-name (,begin-variable-name ,end-variable-name thunk)
                           (bind ((,coordinate-name (make-dimension-coordinate-range ,dimension-variable-name ,begin-variable-name ,end-variable-name)))
                             (funcall thunk)))))
                     `((def (special-variable e) ,coordinate-name
                           ,(if default-coordinate?
                                default-coordinate
                                +whole-domain-marker+))
                       (def (macro e) ,with-macro-name (,name &body forms)
                         `(,',call-with-fn-name ,,name (lambda () ,@forms)))
                       (def (function e) ,call-with-fn-name (,name thunk)
                         (bind ((,coordinate-name ,name))
                           (funcall thunk)))))))))

(def (definer e :available-flags "ioed") dimensional-function ()
  (bind ((name (elt -whole- 2))
         (arguments (elt -whole- 3))
         (key-start-position (position '&key arguments))
         (key-end-position (position '&aux arguments))
         (key-arguments (when key-start-position
                          (subseq arguments (1+ key-start-position) key-end-position)))
         (coordinates-start-position (aprog1 (position '&coordinate arguments) (assert it)))
         (coordinates-end-position (or key-start-position key-end-position))
         (coordinate-arguments (subseq arguments (1+ coordinates-start-position) coordinates-end-position))
         (new-key-arguments (mapcar (lambda (dimension-name)
                                      (bind ((coordinate-name (format-symbol *package* "*~A*" dimension-name)))
                                        `((,(intern (symbol-name dimension-name) :keyword)
                                            ,coordinate-name)
                                          ,coordinate-name)))
                                    coordinate-arguments))
         (whole (list* 'def 'dimensional-function name
                       (append (subseq arguments 0 coordinates-start-position)
                               (list '&key)
                               new-key-arguments
                               key-arguments
                               (when key-end-position
                                 (subseq arguments key-end-position)))
                       (nthcdr 4 -whole-))))
    (cl-def::function-like-definer -definer- 'defun whole -environment- -options-)))

(def function dependent-object-name (dimension-name)
  (format-symbol *package* "~A-DEPENDENT-OBJECT" dimension-name))

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

(def (function e) coordinate (dimension)
  (bind ((dimension (lookup-dimension dimension))
         (name (coordinate-name-of dimension))
         (value (symbol-value name)))
    (if (default-coordinate-marker-p value)
        (aif (default-coordinate-of dimension)
             (funcall it)
             (error "The coordinate for ~A is unspecified" dimension))
        value)))

(def (function e) make-dimension-coordinate-range (dimension begin-or-range &optional (end begin-or-range))
  (bind ((dimension (lookup-dimension dimension))
         (type (the-type-of dimension)))
    (etypecase dimension
      (inheriting-dimension
       (make-ii-coordinate-range (coerce-to-coordinate-begin begin-or-range type)
                                 (coerce-to-coordinate-end end type)))
      (ordering-dimension
       (make-ie-coordinate-range (coerce-to-coordinate-begin begin-or-range type)
                                 (coerce-to-coordinate-end end type))))))

(def (function e) coordinate-begin (dimension)
  (coordinate-range-begin (coordinate dimension)))

(def (function e) coordinate-end (dimension)
  (coordinate-range-end (coordinate dimension)))

(def (function e) domain (dimension)
  ;; TODO domain of ordering-dimensions is the range: [min,max]
  (assert (not (typep dimension 'ordering-dimension)))
  (assert (persistent-class-name-p (the-type-of dimension)))
  (bind ((name (name-of dimension))
         (bulk-name (format-symbol (symbol-package name) "~A-DOMAIN" name)))
    (or (cached-bulk-of bulk-name)
        (setf (cached-bulk-of bulk-name)
              (select (:prefetch-mode :none) (instance)
                      (from (instance))
                      (where (typep instance (the-type-of dimension))))))))

(def (with-macro e) with-coordinate (dimension coordinate)
  (progv
      (list (coordinate-name-of dimension))
      (list coordinate)
    (-body-)))

(def (with-macro e) with-coordinates (dimensions coordinates)
  (progv
      (mapcar #'coordinate-name-of dimensions)
      coordinates
    (-body-)))

