;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;;;;;
;;; Constants

(def (constant e :test 'local-time=) +beginning-of-time+ (parse-timestring "1000-01-01TZ")
  "All dates and timestamps for temporal and time dependent slots are equal or greater than the beginning of time.")

(def (constant e :test 'local-time=) +end-of-time+ (parse-timestring "3000-01-01TZ")
  "All dates and timestamps for temporal and time dependent slots are equal or less than the end of time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Temporal and time dependent

(def (special-variable :documentation "The time machine parameter, modifications made after *t* will not be visible when accessing temporal values.")
    *t*)

(def (special-variable :documentation "When a slot value is time dependent then the approximation uses constant values given for time ranges.")
    *validity-start* +beginning-of-time+)

(def (special-variable :documentation "When a slot value is time dependent then the approximation uses constant values given for time ranges.")
    *validity-end* +end-of-time+)

(def (special-variable :documentation "Specifies whether an unbound-slot-error will be thrown when a time dependent slot value is queried and the value is not fully specified for the current validity range.")
    *signal-unbound-error-for-time-dependent-slots* #t)

;;;;;;;;;;;
;;; Public 

(macrolet ((with-partial-date (date &body forms)
             (rebinding (date)
               `(bind ((date-string
                        (if (typep ,date 'local-time)
                            (format-datestring ,date)
                            ,date)))
                 (ecase (ecase (count #\- date-string)
                          (0 :year)
                          (1 :month)
                          (2 :day))
                   ,@forms)))))
  (defun date-of-first-day-for-partial-date (date)
    (with-partial-date date
      (:year (strcat date-string "-01-01"))
      (:month (strcat date-string "-01"))
      (:day date-string)))

  (defun date-of-last-day-for-partial-date (date)
    (with-partial-date date
      (:year (strcat date-string "-12-31"))
      (:month (let ((date (parse-datestring (strcat date-string "-01"))))
                (with-decoded-local-time (:month month :year year) date
                  (setf date (encode-local-time 0 0 0 0 1 (1+ month) year :timezone +utc-zone+))
                  (format-datestring
                   (local-time-adjust-days date -1)))))
      (:day date-string))))

;; TODO: maybe this should not be part of the public API
;; TODO: add a keyword argument to with-transaction instread
(defmacro with-t (timestamp &body forms)
  `(let ((*t* ,(if (stringp timestamp)
                   `(load-time-value (parse-timestring ,timestamp))
                   timestamp)))
    ,@forms))

(defmacro with-default-t (&body forms)
  `(with-t (transaction-timestamp)
    ,@forms))

(def (function e) call-with-validity-range (start end trunk)
    (bind ((*validity-start* start)
           (*validity-end* end))
      (assert (local-time<= start end))
      (funcall trunk)))

(defmacro with-validity (validity &body forms)
  (assert (not (stringp (first forms))) nil
          "Evaluating the atom ~S in the body of with-validity doesn't make too much sense, you probably would like to use with-validity-range instead"
          (first forms))
  `(call-with-validity-range (load-time-value (parse-datestring ,(date-of-first-day-for-partial-date validity)))
                             (load-time-value (parse-datestring ,(date-of-last-day-for-partial-date validity)))
                             (lambda ()
                               ,@forms)))

(defmacro with-validity-range (start end &body forms)
  `(call-with-validity-range ,(if (stringp start)
                                  `(load-time-value (parse-datestring ,(date-of-first-day-for-partial-date start)))
                                  start)
                             ,(if (stringp end)
                                  `(load-time-value (parse-datestring ,(date-of-last-day-for-partial-date end)))
                                  end)
                             (lambda ()
                               ,@forms)))
