;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2008 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-syntax-sugar)

(register-readtable-for-swank
 '("CL-PEREC" "CL-PEREC-TEST") 'setup-readtable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime inspector integration

#+slime-dwim-branch
(progn
  (defmethod swank::inspect-slot-for-emacs ((class persistent-class)
                                            (instance persistent-object)
                                            (slot persistent-effective-slot-definition))
    (if (debug-persistent-p instance)
        `(,@(if (slot-value-cached-p instance slot)
                `("Cached, value is " (:value ,(standard-instance-access instance (slot-definition-location slot)))
                  " "
                  (:action "[invalidate cache]" ,(lambda () (invalidate-cached-slot instance slot))))
                `("Not cached"
                  " "
                  (:action "[read in]" ,(lambda () (slot-value-using-class class instance slot)))))
          " "
          (:action "[make unbound]" ,(lambda () (slot-makunbound-using-class class instance slot))))
        (call-next-method)))

  (defmethod swank::inspect-for-emacs ((instance persistent-object) inspector)
    (bind ((result (multiple-value-list (call-next-method))))
      (if (= (length result) 1)
          (progn
            (setf result (first result))
            (bind ((content (getf result :content)))
              (setf (getf result :content)
                    (append `("Transaction: " (:value ,(when (instance-in-transaction-p instance) (transaction-of instance))) (:newline))
                            content))
              (setf (getf result :title)
                    (if (debug-persistent-p instance) "A persistent instance" "A transient instance"))
              result))
          ;; we do nothing with the old inspect protocol...
          (values-list result)))))
