(in-package :cl-perec)

;;;;;;;;;;;;;;;;
;;; Base classes

(defpclass* t-object ()
  ()
  (:abstract #t))

(defpclass* h-object ()
  ()
  (:abstract #t))

(defassociation*
  ((:class h-object :slot t-object :type t-object)
   (:class t-object :slot h-objects :type (set h-object))))

;;;;;;;;;;;;;;
;;; Timestamps

(defpclass* temporal-object ()
  ((t-value :type timestamp))
  (:abstract #t))

(defpclass* time-dependent-object ()
  ((validity-start :type timestamp)
   (validity-end :type timestamp))
  (:abstract #t))
