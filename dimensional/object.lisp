(in-package :cl-perec)

;;;;;;;;;;;;;;;;
;;; Base classes

(defpclass* persistent-object-d ()
  ()
  (:abstract #t))

(defpclass* persistent-object-h ()
  ()
  (:abstract #t))

(defassociation*
  ((:class persistent-object-h :slot d-instance :type persistent-object-d)
   (:class persistent-object-d :slot h-instances :type (set persistent-object-h))))
