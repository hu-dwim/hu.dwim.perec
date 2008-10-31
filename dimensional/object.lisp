(in-package :cl-perec)

;;;;;;;;;;;;;;;;
;;; Base classes

(defpclass* persistent-object-d ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defpclass* persistent-object-h ()
  ()
  (:abstract #t)
  (:direct-store :push-down))

(defassociation*
  ((:class persistent-object-h :slot d-instance :type persistent-object-d)
   (:class persistent-object-d :slot h-instances :type (set persistent-object-h))))
