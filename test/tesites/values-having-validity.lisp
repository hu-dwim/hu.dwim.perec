(in-package :cl-perec-test)

;; TODO:
#+nil
(defun test-values-having-validity ()
  (extract-values-having-validity-range
   (make-instance 'values-having-validity
                  :values (make-array 2 :initial-contents '(1000 2000))
                  :validity-starts (make-array 2 :initial-contents (list (parse-datestring "2006-01-01")
                                                                         (parse-datestring "2007-01-01")))
                  :validity-ends (make-array 2 :initial-contents (list (parse-datestring "2006-12-31")
                                                                       (parse-datestring "2007-12-31"))))
   (parse-datestring "2006-06-06")
   (parse-datestring "2007-07-07")))
