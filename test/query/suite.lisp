;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec.test)

(defvar *show-query* nil)

(defmacro with-sql-recording (&body body)
  `(unwind-protect
    (progn
      (start-sql-recording)
      ,@body)
    (stop-sql-recording)))

(defmacro with-debug-query-compiler (&body body)
  `(let ((*test-query-compiler* #t))
     ,@body))

(defmacro without-debug-query-compiler (&body body)
  `(let ((*test-query-compiler* #f))
     ,@body))

(defmacro with-record-count-check (count &body body)
  (if count
      (with-unique-names (result)
        `(bind ((,result (progn ,@body)))
          (is (= (length ,result) ,count))
          ,result))
      `(progn ,@body)))

(defmacro with-select-count-check (count &body body)
  (if count
      (with-unique-names (start result end)
        `(bind ((,start (hu.dwim.perec::select-counter-of (command-counter-of *transaction*)))
                (,result (progn ,@body))
                (,end (hu.dwim.perec::select-counter-of (command-counter-of *transaction*))))
          (is (= (- ,end ,start) ,count))
          ,result))
      `(progn ,@body)))

(defmacro test-query ((&key (select-count 1) (record-count nil) (fixture nil)) &body forms)
  `(finishes
    (with-setup ,fixture
      (run-queries
        (without-debug-query-compiler
          (with-select-count-check ,select-count
            (with-record-count-check ,record-count
              ,@forms)))
        (with-debug-query-compiler
          ,@forms)))))

(defun run-query-tests ()
  (with-sql-recording
    (let ((*show-query* #t)
          (*debug-on-assertion-failure* #f)
          (*debug-on-unexpected-error* #f))
      (test/query))))

(defun debug-query-test (test)
  (with-sql-recording
    (let ((*show-query* #t))
      (funcall test))))

(defmacro run-queries (&body queries)
  `(with-transaction
    (when *show-query*
      (format t "~{~&~A~}" ',queries))
    ,@queries))

(defun first-arg (arg-1 &rest rest-args)
  (declare (ignore rest-args))
  arg-1)

