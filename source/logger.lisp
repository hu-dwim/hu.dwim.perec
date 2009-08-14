;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *runtime-log-level* (if *load-as-production?* +info+ +debug+))
  (defvar *compile-time-log-level* (if *load-as-production?* +debug+ +dribble+)))

(deflogger plog ()
  :level *runtime-log-level*
  :compile-time-level *compile-time-log-level*
  :appenders ((debug-only*
                (make-instance 'brief-stream-log-appender :stream *debug-io*))))

(deflogger qlog ()
  :level *runtime-log-level*
  :compile-time-level *compile-time-log-level*
  :appenders ((debug-only*
                (make-instance 'brief-stream-log-appender :stream *debug-io*))))
