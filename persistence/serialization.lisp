(in-package :cl-perec)

(defvar +persistent-object-code+
  (register-code 131 'persistent-object nil))

(defstore-cl-store (obj persistent-object stream)
  (output-type-code +persistent-object-code+ stream)
  (cl-store::store-type-object obj stream))

(defrestore-cl-store (persistent-object stream)
  (load-object (oid-of (cl-store::restore-type-object stream)) :skip-existence-check #t))

(defmethod serializable-slots-using-class ((object persistent-object) class)
  (list (find-slot (class-name class) 'oid)))
