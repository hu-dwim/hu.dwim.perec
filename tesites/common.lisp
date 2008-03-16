;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(def function complete-partial-timestamp (timestamp)
  (bind (((partial-type completion)
          (ecase (count #\- timestamp)
            (0 '(:year "-01-01T00:00:00Z"))
            (1 '(:month "-01T00:00:00Z"))
            (2 (if (search "T" timestamp)
                   (ecase (count #\: timestamp)
                     (0 '(:hour ":00:00Z"))
                     (1 '(:minute ":00Z"))
                     (2 `(:sec ,(if (ends-with timestamp "Z") "" "Z"))))
                   '(:day "T00:00:00Z"))))))
    (values (strcat timestamp completion) partial-type)))

(def function first-moment-for-partial-timestamp (timestamp)
  (complete-partial-timestamp timestamp))

(def function last-moment-for-partial-timestamp (timestamp)
  (bind (((:values timestamp-string timestamp-partial-type) (complete-partial-timestamp timestamp))
         ((:values nsecond second minute hour day month year day-of-week daylight-saving-time-p timezone) (decode-local-time (parse-timestring timestamp-string))))
    (declare (ignore day-of-week daylight-saving-time-p))
    (flet ((1+* (partial-type value)
             (if (eq partial-type timestamp-partial-type)
                 (1+ value)
                 value))
           (1-0 (partial-type)
             (if (eq partial-type timestamp-partial-type)
                 1
                 0)))
      (format-timestring
       (local-time+
        (encode-local-time nsecond second minute hour
                           (1+* :day day) (1+* :month month) (1+* :year year)
                           :timezone timezone)
        (encode-duration :hour (1-0 :hour) :minute (1-0 :minute) :sec (1-0 :sec)))))))
