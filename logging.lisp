;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *runtime-log-level* (if *load-as-production-p* +info+ +debug+))
  (defvar *compile-time-log-level* (if *load-as-production-p* +debug+ +dribble+)))

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
