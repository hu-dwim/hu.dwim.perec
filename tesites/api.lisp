;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;
;;; Constants

(def (load-time-constant e) +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  "All timestamps for temporal and time dependent slots are equal or greater than the beginning of time. Basically there should be no timestamp before the beginning of time.")

(def (load-time-constant e) +end-of-time+ (parse-timestring "3000-01-01TZ")
  "All timestamps for temporal and time dependent slots are equal or less than the end of time. Basically there should be no timestamp after the end of time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporal and time dependent

(def (special-variable :documentation "The time machine parameter, modifications made in the database after *t* will not be visible when accessing temporal values. This parameter can be set only once for a transaction.")
    *t*)

(def (special-variable :documentation "When dealing with time dependent slots this is the start timestamp of the value's validity. The time dependent approximation uses constant values for time intervals.")
    *validity-start* +beginning-of-time+)

(def (special-variable :documentation "When dealing with time dependent slots this is the end timestamp of the value's validity. The time dependent approximation uses constant values for time intervals.")
    *validity-end* +end-of-time+)

;;;;;;;;;;;;;
;;; Arguments

(def function default-t ()
  (transaction-timestamp))

(def (function e) call-with-t (t-value thunk)
  (assert (not (boundp '*t*)) nil "Changing the time machine parameter *t* is not allowed within a single transaction.")
  (bind ((*t* t-value))
    (assert (eq +utc-zone+ (timezone-of t-value)))
    (funcall thunk)))

(def (macro e) with-t (timestamp &body forms)
  `(call-with-t
    ,(if (stringp timestamp)
         `(load-time-value (parse-timestring ,timestamp))
         timestamp)
    (lambda ()
      ,@forms)))

(def (macro e) with-default-t (&body forms)
  `(with-t (default-t)
     ,@forms))

(def (function e) call-with-validity-range (start end thunk)
  (bind ((*validity-start* start)
         (*validity-end* end))
    (assert (and (eq +utc-zone+ (timezone-of start))
                 (eq +utc-zone+ (timezone-of end))
                 (local-time< start end)))
    (funcall thunk)))

(def (macro e) with-validity (validity &body forms)
  (assert (not (stringp (first forms))) nil
          "Evaluating the atom ~S in the body of with-validity doesn't make too much sense, you probably would like to use with-validity-range instead"
          (first forms))
  `(call-with-validity-range
    (load-time-value (parse-timestring ,(first-moment-for-partial-timestamp validity)))
    (load-time-value (parse-timestring ,(last-moment-for-partial-timestamp validity)))
    (lambda ()
      ,@forms)))

(def (macro e) with-validity-range (start end &body forms)
  `(call-with-validity-range
    ,(if (stringp start)
         `(load-time-value (parse-timestring ,(first-moment-for-partial-timestamp start)))
         start)
    ,(if (stringp end)
         `(load-time-value (parse-timestring ,(first-moment-for-partial-timestamp end)))
         end)
    (lambda ()
      ,@forms)))
