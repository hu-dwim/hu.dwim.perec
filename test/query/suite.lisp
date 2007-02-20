(in-package :cl-perec-test)

(defsuite* (test/query :in test))

(defvar *show-query* nil)

(defmacro with-sql-recording (&body body)
  `(unwind-protect
    (progn
      (start-sql-recording)
      ,@body)
    (stop-sql-recording)))

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

(defmacro test-query ((&key (select-count 1) (record-count nil) (fixture nil)) &body forms)
  `(finishes
    ,(when fixture `(funcall ',fixture))
    (run-queries
      ,(when select-count
             `(progn
               (let ((counter-start (prc::select-counter-of (command-counter-of *transaction*))))
                 (let ((prc::*test-query-compiler* #f))
                   ,@forms)
                 (is (= (- (prc::select-counter-of (command-counter-of *transaction*))
                           counter-start)
                        ,select-count)))))
      (bind ((result (let ((prc::*test-query-compiler* #t)) ,@forms)))
        ,(if record-count
             `(is (= (length result) ,record-count))
             `(is (not (null result))))))))

(defun first-arg (arg-1 &rest rest-args)
  (declare (ignore rest-args))
  arg-1)

