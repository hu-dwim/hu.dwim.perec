(in-package :cl-perec)

;;;;;;;;;;;;;;;;
;;; Base classes

(defpclass* d-object ()
  ()
  (:abstract #t))

(defpclass* h-object ()
  ()
  (:abstract #t))

(defassociation*
  ((:class h-object :slot d-object :type d-object)
   (:class d-object :slot h-objects :type (set h-object))))

