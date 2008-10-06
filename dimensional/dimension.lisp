;;;;;;;;
;;;; egyszerűsített dimenzió alapú ebr42 model

(in-package :cl-perec)

(def class* dimension ()
  ((name
    :type symbol)
   (persistent-type ;; FIXME should be type, but accessor collides with cl:type-of
    :initarg :type
    :type persistent-type)
   (ordered
    #f
    :type boolean)
   (inherit
    nil
    :type (member nil :ascending :descending))))

(def print-object dimension
    (write (name-of -self-)))

(def special-variable *dimensions* nil)

(def function find-dimension (name &key (otherwise :error))
  (or (find name *dimensions* :key #'name-of)
      (handle-otherwise otherwise)))

(def definer dimension (name &key type ordered inherit)
  (bind ((dependent-object-name (format-symbol *package* "~A-DEPENDENT-OBJECT" name))
         (begin-variable-name (format-symbol *package* "~A-BEGIN" name))
         (begin-special-name (format-symbol *package* "*~A*" begin-variable-name))
         (end-variable-name (format-symbol *package* "~A-END" name))
         (end-special-name (format-symbol *package* "*~A*" end-variable-name))
         (with-macro-name (format-symbol *package* "WITH-~A" name))
         (with-range-macro-name (format-symbol *package* "WITH-~A-RANGE" name))
         (call-with-fn-name (format-symbol *package* "CALL-~A" with-macro-name))
         (call-with-range-fn-name (format-symbol *package* "CALL-~A" with-range-macro-name))
         (slots (if (and ordered (not inherit))
                    `((,begin-variable-name :type ,type)
                      (,end-variable-name :type ,type))
                    `((,name :type ,type)))))
    `(progn
       (bind ((dimension (make-instance 'dimension :name ',name :type ',type :ordered ,ordered :inherit ,inherit)))
         (aif (find (name-of dimension) *dimensions* :key #'name-of)
              (setf *dimensions* (nsubstitute dimension it *dimensions*))
              (push dimension *dimensions*)))
       (defpclass* ,dependent-object-name ()
         ,slots
         (:abstract #t))
       (def special-variable ,begin-special-name)
       (def special-variable ,end-special-name)
       (def macro ,with-macro-name (,name &body forms)
         `(,',call-with-fn-name
           ,(coerce-to-dimension ,name ',type)
           (lambda () ,@forms)))
       (def function ,call-with-fn-name (,name thunk)
         (bind ((,begin-special-name ,name)
                (,end-special-name ,name))
           (funcall thunk)))
       ,@(when ordered
               `((def macro ,with-range-macro-name (,begin-variable-name ,end-variable-name &body forms)
                   `(,',call-with-range-fn-name
                     ,(coerce-to-dimension-begin ,begin-variable-name ',type)
                     ,(coerce-to-dimension-end ,end-variable-name ',type)
                     (lambda () ,@forms)))
                 (def function ,call-with-range-fn-name (,begin-variable-name ,end-variable-name thunk)
                   (bind ((,begin-special-name ,begin-variable-name)
                          (,end-special-name ,end-variable-name))
                     (funcall thunk))))))))

(def function coerce-to-dimension (form type)
  (case type
    (timestamp (if (stringp form) (parse-timestring form) form))
    (t form)))

(def function coerce-to-dimension-begin (form type)
  (case type
    (timestamp (if (stringp form) (first-moment-for-partial-timestamp form) form))
    (t form)))

(def function coerce-to-dimension-end (form type)
  (case type
    (timestamp (if (stringp form) (last-moment-for-partial-timestamp form) form))
    (t form)))

(def dimension time :type timestamp :ordered #t :inherit :ascending)

(def dimension validity :type timestamp :ordered #t)


