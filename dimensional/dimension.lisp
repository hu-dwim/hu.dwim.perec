;;;;;;;;
;;;; egyszerűsített dimenzió alapú ebr42 model

(in-package :cl-perec)

(def class* dimension ()
  ((name
    :type symbol)
   (persistent-type ;; FIXME should be type, but accessor collides with cl:type-of
    :type persistent-type)
   (ordered
    #f
    :type boolean)
   (inherit
    nil
    :type (member nil :ascending :descending))))

(def special-variable *dimensions* nil)

(def function find-dimension (name &key (otherwise :error))
  (or (find name *dimensions* :key #'name-of)
      (handle-otherwise otherwise)))

(def definer dimension (name &key type ordered inherit)
  (bind ((dependent-object-name (format-symbol *package* "~A-DEPENDENT-OBJECT" name))
         (begin-variable-name (format-symbol *package* "~A-BEGIN" name))
         (end-variable-name (format-symbol *package* "~A-END" name))
         (with-macro-name (format-symbol *package* "WITH-~A" name))
         (with-range-macro-name (format-symbol *package* "WITH-~A-RANGE" name))
         (slots (if (and ordered (not inherit))
                    `((,begin-variable-name :type ,type)
                      (,end-variable-name :type ,type))
                    `((,name :type ,type)))))
    `(progn
      (push (make-instance 'dimension :persistent-type ,type :ordered ,ordered :inherit ,inherit) *dimensions*)
      (def pclass* ,dependent-object-name ()
        ,slots)
      (def special-variable ,(format-symbol *package* "*~A*" begin-variable-name))
      (def special-variable ,(format-symbol *package* "*~A*" end-variable-name))
      (def macro ,with-macro-name (,name &body forms)
        ;;TODO)
      (def macro ,with-range-macro-name (,begin-variable-name ,end-variable-name &body forms)
        ;;TODO))))

(def dimension time :type timestamp :ordered #t :inherit :ascending)

(def dimension validity :type timestamp :ordered #t)

