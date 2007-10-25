;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

(defvar *log-level* (if *load-as-production-p* +info+ +debug+))
(defvar *compile-time-log-level* (if *load-as-production-p* +debug+ +dribble+))

(deflogger qlog ()
  :level *log-level*
  :compile-time-level *compile-time-log-level*
  :appenders ((debug-only*
                (make-instance 'brief-stream-log-appender :stream *debug-io*))))
