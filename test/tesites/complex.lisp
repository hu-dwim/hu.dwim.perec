;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

(defpclass* tesites-complex-test ()
  ((slot :type (or null integer-32))
   (temporal-slot :type (or null integer-32) :temporal #t)
   (time-dependent-slot :type (or null integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or null integer-32) :temporal #t :time-dependent #t)))

(defvar *history-entries*)

(defstruct (history-entry (:conc-name he-))
  (instance nil :type (or null persistent-object))
  (slot-name nil :type symbol)
  (t-value nil :type (or null local-time))
  (validity-start nil :type (or null local-time))
  (validity-end nil :type (or null local-time))
  (value nil :type t))

(defun complext-test-slot-names (instance)
  (iter (for slot :in (prc::persistent-effective-slots-of (class-of instance)))
        (when (primitive-type-p* (prc::canonical-type-of slot))
          (collect (slot-definition-name slot)))))

(defun slot-value* (instance slot-name)
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name))
         (temporal-p (and (typep slot 'persistent-effective-slot-definition-t)
                          (prc::temporal-p slot)))
         (time-dependent-p (and (typep slot 'persistent-effective-slot-definition-t)
                                (prc::time-dependent-p slot)))
         (history-entries (collect-if (lambda (entry)
                                        (and (equal (oid-of instance) (oid-of (he-instance entry)))
                                             (eq slot-name (he-slot-name entry))))
                                      *history-entries*)))
    (when history-entries
      (cond ((and (not temporal-p)
                  (not time-dependent-p))
             (he-value (first history-entries)))
            ((and temporal-p
                  time-dependent-p)
             (prc::collect-values-having-validity history-entries #'he-value #'he-validity-start #'he-validity-end (constantly nil) *validity-start* *validity-end*))
            (temporal-p
             (he-value (find-if #L(local-time< (he-t-value !1) *t*) history-entries)))
            (time-dependent-p
             (prc::collect-values-having-validity history-entries #'he-value #'he-validity-start #'he-validity-end (constantly nil) *validity-start* *validity-end*))))))

(defun (setf slot-value*) (new-value instance slot-name)
  (push (make-history-entry :instance instance
                            :slot-name slot-name
                            :t-value *t*
                            :validity-start *validity-start*
                            :validity-end *validity-end*
                            :value new-value)
        *history-entries*))

(defun generate-instances (&optional (count 1))
  (iter (repeat count)
        (collect (make-instance 'tesites-complex-test))))

(defun compare-history (instances)
  (sort *history-entries* #'local-time< :key #'he-t-value)
  (iter (for instance :in instances)
        (for class = (class-of instance))
        (revive-instance instance)
        (iter (for slot-name :in (complext-test-slot-names instance))
              (for persistent-value = (slot-value instance slot-name))
              (for test-value = (slot-value* instance slot-name))
              (is (or (and (values-having-validity-p persistent-value)
                           (values-having-validity-p test-value)
                           (iter (for (persistent-value persistent-validity-start persistent-validity-end) :in-values-having-validity persistent-value)
                                 (for (test-value test-validity-start test-validity-end) :in-values-having-validity test-value)
                                 (always (and (eql persistent-value test-value)
                                              (local-time= persistent-validity-start test-validity-start)
                                              (local-time= persistent-validity-end test-validity-end)))))
                      (eql persistent-value test-value))
                  "The persistent value: ~A and test value: ~A are different~%in the slot ~A of ~A~%with t ~A and with validity range ~A -> ~A"
                  persistent-value test-value slot-name instance *t* *validity-start* *validity-end*))))

(defun random-universal-time ()
  (random 1000000000))

(defmacro with-random-t (&body forms)
  `(with-t (local-time :universal (random-universal-time))
     ,@forms))

(defmacro with-random-validity-range (&body forms)
  `(bind ((start-universal (random-universal-time))
          (validity-start (local-time :universal start-universal))
          (validity-end (local-time :universal (+ start-universal (random-universal-time)))))
     (with-validity-range validity-start validity-end
       ,@forms)))

(defun do-random-operations (instances &optional (count 1))
  (with-random-t
    (with-random-validity-range
      (iter (repeat count)
            (bind ((instance (revive-instance (elt instances (random (length instances)))))
                   (slot-names (complext-test-slot-names instance))
                   (slot-name (elt slot-names (random (length slot-names))))
                   (value (random 100)))
              (format t "~%Setting ~A in ~A~% with t ~A and with validity range ~A -> ~A~%to ~A"
                      slot-name instance *t* *validity-start* *validity-end* value)
              (setf (slot-value instance slot-name) value)
              (setf (slot-value* instance slot-name) value))))))

(deftest (test/tesites/complex :in test/tesites) (&key (instance-count 1) (operation-count 1) (repeat-count 1) (test-count 1))
  (bind ((*history-entries* nil)
         (instances
          (with-transaction
            (with-default-t
              (generate-instances instance-count)))))
    (format t "~%Starting operations...")
    (iter (repeat repeat-count)
          (with-transaction
            (do-random-operations instances operation-count))
          (finally
           (with-transaction
             (with-default-t
               (compare-history instances)))
           (iter (repeat test-count)
                 (with-transaction
                   (with-random-t
                     (with-random-validity-range
                       (compare-history instances)))))))))
