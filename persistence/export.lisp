(in-package :cl-perec)

(def constant +persistent-object-code+ #x61)

(def (function e) export-persistent-instances (object stream persistent-object-serializer)
  (serializer:serialize object
                        :output stream
                        :serializer-mapper (make-export-serializer-mapper
                                            persistent-object-serializer)))

(def (function e) import-persistent-instances (stream persistent-object-deserializer)
  (serializer:deserialize stream
                          :deserializer-mapper (make-export-deserializer-mapper
                                                persistent-object-deserializer)))

(def (function o) make-export-serializer-mapper (persistent-object-serializer)
  (lambda (object context)
    (bind (((values code has-identity writer-function)
            (cl-serializer::default-serializer-mapper object context)))
      (if (and (eq code serializer::+standard-object-code+)
               (typep object 'persistent-object))
          (values +persistent-object-code+ #t
                  (lambda (object context)
                    (bind ((class (class-of object)))
                      (serializer::serialize-symbol (class-name class) context)
                      (funcall persistent-object-serializer object context))))
          (values code has-identity writer-function)))))

(def (function o) make-export-deserializer-mapper (persistent-object-deserializer)
  (lambda (code context)
    (if (eq code +persistent-object-code+)
       (lambda (context &optional referenced)
         (declare (ignore referenced))
         (bind ((class-name (serializer::deserialize-symbol context))
                (class (find-class class-name :errorp #f))
                (prototype-or-class-name (or (and class (sb-pcl:class-prototype class))
                                             class-name)))
           (funcall persistent-object-deserializer prototype-or-class-name context)))
       (cl-serializer::default-deserializer-mapper code context))))

(def (function e) write-persistent-object-slot-values (object context)
  (bind ((class (class-of object))
         (slots (collect-if #'persistent-slot-p (closer-mop:class-slots class))))
    (serializer::write-variable-length-positive-integer (length slots) context)
    (dolist (slot slots)
      (unless (eq (closer-mop:slot-definition-allocation slot) :class)
        (serializer::serialize-symbol (closer-mop:slot-definition-name slot) context)
        (if (closer-mop:slot-boundp-using-class class object slot)
            (serializer::serialize-element (closer-mop:slot-value-using-class class object slot) context)
            (serializer::write-unsigned-byte-8 serializer::+unbound-slot-code+ context))))))

(def (function e) read-persistent-object-slot-values (prototype-or-class-name context)
  (bind ((class (etypecase prototype-or-class-name
                  (symbol (find-class prototype-or-class-name))
                  (persistent-object (class-of prototype-or-class-name))))
         (object (allocate-instance class)))
    (serializer::announce-identity object context)
    (loop repeat (the fixnum (serializer::read-variable-length-positive-integer context))
      for slot-name = (serializer::deserialize-symbol context) do
      (if (eq serializer::+unbound-slot-code+ (serializer::read-unsigned-byte-8 context))
          (slot-makunbound object slot-name)
          (setf (slot-value object slot-name)
                (progn
                  (serializer::unread-unsigned-byte-8 context)
                  (serializer::deserialize-element context)))))
    (make-persistent object)
    object))