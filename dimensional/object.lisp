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
  ((:class h-object :slot d-instance :type d-object)
   (:class d-object :slot h-instances :type (set h-object))))
