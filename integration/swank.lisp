;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.perec)

;;;;;;
;;; Slime inspector integration

#+hu.dwim.slime
(progn

(defmethod swank::slot-value-for-inspector ((class persistent-class)
                                            (instance persistent-object)
                                            (slot persistent-effective-slot-definition))
  (if (debug-persistent-p instance)
      `(,@(if (slot-value-cached-p instance slot)
              `((:value ,(standard-instance-access instance (slot-definition-location slot)))
                " "
                (:action "[invalidate cache]" ,(lambda () (invalidate-cached-slot instance slot))))
              `("#<not cached>"
                " "
                (:action "[read in]" ,(lambda () (slot-value-using-class class instance slot))))))
      (call-next-method)))

(defmethod swank-backend::emacs-inspect ((instance persistent-object))
  (flet ((annotate (content)
           (append (if (debug-persistent-p instance)
                       (if (instance-in-transaction-p instance)
                           `("A persistent instance in transaction " (:value ,(transaction-of instance)) ".")
                           `("A detached persistent instance."))
                       "A transient instance")
                   `((:newline) (:newline))
                   content)))
    (bind ((result (call-next-method)))
      (if (and (consp result)
               (keywordp (first result)))
          (progn
            (setf result (copy-list result))
            (setf (getf result :content) (annotate (getf result :content))))
          (setf result (annotate result)))
      result)))
)
