(in-package :cl-perec)

;;;;;;;;;;;;;;;;
;;; Base classes

(defpclass* d-object ()
  (#+nil(last-consolidated-version :type integer-64))
  (:abstract #t))

(defpclass* h-object ()
  (#+nil(version :type integer-64))
  (:abstract #t))

(defassociation*
  ((:class h-object :slot d-instance :type d-object)
   (:class d-object :slot h-instances :type (set h-object))))
