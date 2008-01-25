(in-package :cl-perec)

(defpclass* t-object ()
  ())

(defpclass* h-object ()
  ())

(defassociation*
  ((:class h-object :slot t-object :type t-object)
   (:class t-object :slot h-objects :type (set h-object))))

(defpclass* temporal-object (h-object)
  ((t-value :type timestamp)))

(defpclass* time-dependent-object (h-object)
  ((validity-start :type timestamp)
   (validity-end :type timestamp)))