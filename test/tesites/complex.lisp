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

(defpclass* tesites-complex-slot-test ()
  ((slot :type (or null integer-32))))

(defpclass* tesites-complex-temporal-slot-test ()
  ((temporal-slot :type (or null integer-32) :temporal #t)))

(defpclass* tesites-complex-time-dependent-slot-test ()
  ((time-dependent-slot :type (or null integer-32) :time-dependent #t)))

(defpclass* tesites-complex-temporal-and-time-dependent-slot-test ()
  ((temporal-and-time-dependent-slot :type (or null integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-inheritance-test
    (tesites-complex-slot-test tesites-complex-temporal-slot-test tesites-complex-time-dependent-slot-test tesites-complex-temporal-and-time-dependent-slot-test)
  ()
  (:metaclass persistent-class-t))

(defvar *history-entry-counter*)

(defvar *history-entries*)

(defstruct (history-entry (:conc-name he-))
  (step (incf *history-entry-counter*) :type integer)
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
                                      *history-entries*))
         (default-value nil))
    (flet ((sort-entries-by-step (entries)
             (stable-sort entries #'> :key #'he-step))
           (sort-entries-by-t (entries)
             (stable-sort entries #'local-time> :key #'he-t-value))
           (validity-range-overlap-p (entry)
             (and (local-time< (he-validity-start entry) *validity-end*)
                  (local-time< *validity-start* (he-validity-end entry)))))
      (when history-entries
        (bind ((value
                (cond ((and (not temporal-p)
                            (not time-dependent-p))
                       (aif (first (sort-entries-by-step history-entries))
                            (he-value it)
                            default-value))
                      ((and temporal-p
                            time-dependent-p)
                       (prc::collect-values-having-validity
                        (sort-entries-by-t
                         (collect-if (lambda (entry)
                                       (and (local-time<= (he-t-value entry) *t*)
                                            (validity-range-overlap-p entry)))
                                     history-entries))
                        #'he-value #'he-validity-start #'he-validity-end (constantly default-value) *validity-start* *validity-end*))
                      (temporal-p
                       (aif (find-if #L(local-time<= (he-t-value !1) *t*)
                                     (sort-entries-by-t history-entries))
                            (he-value it)
                            default-value))
                      (time-dependent-p
                       (prc::collect-values-having-validity
                        (sort-entries-by-step
                         (collect-if (lambda (entry)
                                       (validity-range-overlap-p entry))
                                     history-entries))
                        #'he-value #'he-validity-start #'he-validity-end (constantly default-value) *validity-start* *validity-end*)))))
          (if (single-values-having-validity-p value)
              (elt-0 (prc::values-of value))
              value))))))

(defun (setf slot-value*) (new-value instance slot-name)
  (assert (not (values-having-validity-p new-value)))
  (push (make-history-entry :instance instance
                            :slot-name slot-name
                            :t-value *t*
                            :validity-start *validity-start*
                            :validity-end *validity-end*
                            :value new-value)
        *history-entries*))

(defun (setf slot-value-and-slot-value*) (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value)
  (setf (slot-value* instance slot-name) new-value))

(defun generate-instances (class-name count)
  (iter (repeat count)
        (for instance = (make-instance class-name))
        (iter (for slot-name :in (complext-test-slot-names instance))
              (for persistent-value = (slot-value instance slot-name))
              (setf (slot-value* instance slot-name) persistent-value))
        (collect instance)))

(defun compare-persistent-and-test-values (persistent-value test-value)
  (or (and (values-having-validity-p persistent-value)
           (values-having-validity-p test-value)
           (iter (for (persistent-value persistent-validity-start persistent-validity-end) :in-values-having-validity persistent-value)
                 (for (test-value test-validity-start test-validity-end) :in-values-having-validity test-value)
                 (always (and (eql persistent-value test-value)
                              (local-time= persistent-validity-start test-validity-start)
                              (local-time= persistent-validity-end test-validity-end)))))
      (eql persistent-value test-value)))

(defun assert-persistent-and-test-values (instance slot-name persistent-value test-value)
  (is (compare-persistent-and-test-values persistent-value test-value)
      "The persistent value: ~A and test value: ~A are different~%in the slot ~A of ~A~%with t ~A and with validity range ~A -> ~A~%with ~A history entries: ~A"
      persistent-value test-value slot-name instance *t* *validity-start* *validity-end* (length *history-entries*) *history-entries*))

(defun compare-history (instances)
  (iter (for instance :in instances)
        (for class = (class-of instance))
        (revive-instance instance)
        (iter (for slot-name :in (complext-test-slot-names instance))
              (for persistent-value = (slot-value instance slot-name))
              (for test-value = (slot-value* instance slot-name))
              (assert-persistent-and-test-values instance slot-name persistent-value test-value))))

(defun full-compare-history (instances)
  (bind ((t-values
          (delete-duplicates (append (list +beginning-of-time+)
                                     (sort (mapcar 'he-t-value *history-entries*) #'local-time<)
                                     (list +end-of-time+))
                             :test #'local-time=))
         (validity-values
          (delete-if #L(or (local-time< !1 +beginning-of-time+)
                           (local-time< +end-of-time+ !1))
                     (sort (mapcan #L(list (adjust-local-time !1 (offset :nsec -1000000))
                                           !1
                                           (adjust-local-time !1 (offset :nsec 1000000)))
                                   (delete-duplicates (append (mapcar 'he-validity-start *history-entries*)
                                                              (mapcar 'he-validity-end *history-entries*))
                                                      :test #'local-time=))
                           #'local-time<))))
    (format t "~&T values: ~A" t-values)
    (format t "~&Validity values: ~A~%" validity-values)
    (iter (with count = 0)
          (with total = (* (length t-values)
                           (/ (* (length validity-values)
                                 (1- (length validity-values)))
                              2)))
          (for t-value :in t-values)
          (iter (for validity-start-list :on validity-values)
                (for validity-start = (car validity-start-list))
                (iter (for validity-end :in (cdr validity-start-list))
                      (when (zerop (mod count 100))
                        (format t "~&At: ~d/~d" count total))
                      (incf count)
                      (with-t t-value
                        (with-validity-range validity-start validity-end
                          (with-transaction
                            (compare-history instances)))))))))

(defun random-universal-time ()
  (random 5000000000))

(defun random-local-time ()
  (local-time :universal (random-universal-time) :timezone +utc-zone+))

(defmacro with-random-t (&body forms)
  `(with-t (random-local-time)
     ,@forms))

(defmacro with-random-validity-range (&body forms)
  `(bind ((start-universal (random-universal-time))
          (validity-start (local-time :universal start-universal :timezone +utc-zone+))
          (validity-end (local-time :universal (+ start-universal (random-universal-time)) :timezone +utc-zone+)))
     (with-validity-range validity-start validity-end
       ,@forms)))

;; TODO: generate partially equal operations in terms of t, validity range
(defun do-random-operations (instances &key (count 1) (slot-names nil))
  (with-random-t
    (with-random-validity-range
      (iter (repeat count)
            (bind ((instance (revive-instance (elt instances (random (length instances)))))
                   (slot-names (or slot-names (complext-test-slot-names instance)))
                   (slot-name (elt slot-names (random (length slot-names))))
                   (value (random 100)))
              (format t "~%Setting ~A in ~A~% with t ~A and with validity range ~A -> ~A~%to ~A"
                      slot-name instance *t* *validity-start* *validity-end* value)
              (setf (slot-value-and-slot-value* instance slot-name) value))))))

(deftest (test/tesites/complex :in test/tesites) (&key (class-name 'tesites-complex-test) (instance-count 1) (operation-count 1) (repeat-count 1)
                                                       (full-test t) (test-count 1) (slot-name nil) (slot-names nil))
  (bind ((*history-entries* nil)
         (*history-entry-counter* 0)
         (error nil)
         (instances
          (with-transaction
            (with-default-t
              (generate-instances class-name instance-count)))))
    (format t "~%Starting operations with ~A number of history entries..." (length *history-entries*))
    (restart-bind
        ((print-test
          (lambda ()
            (bind ((*print-level* nil)
                   (*print-length* nil)
                   (*print-lines* nil)
                   (failure-descriptions (stefil::failure-descriptions-of stefil::*global-context*))
                   (failure-description (aref failure-descriptions (1- (length failure-descriptions))))
                   (format-arguments (stefil::format-arguments-of failure-description))
                   (slot-name (elt format-arguments 2))
                   (instance (elt format-arguments 3)))
              (format t "~%~S"
                      `(deftest test/tesites/complex/generated ()
                         ,(format nil "~A" error)
                         (bind ((*history-entries* nil)
                                (*history-entry-counter* 0)
                                (instance
                                 (with-transaction
                                   (with-default-t
                                     (make-instance ',(class-name (class-of instance)))))))
                           (with-transaction
                             (with-revived-instance instance
                               ,@(iter (for entry :in (reverse *history-entries*))
                                       (collect `(with-t ,(format-timestring (he-t-value entry) :timezone +utc-zone+)
                                                   (with-validity-range
                                                       ,(format-timestring (he-validity-start entry) :timezone +utc-zone+)
                                                       ,(format-timestring (he-validity-end entry) :timezone +utc-zone+)
                                                     (setf (slot-value-and-slot-value* instance ',(he-slot-name entry)) ,(he-value entry))))))))
                           (with-transaction
                             (with-revived-instance instance
                               (with-t ,(format-timestring *t* :timezone +utc-zone+)
                                 (with-validity-range
                                     ,(format-timestring *validity-start* :timezone +utc-zone+)
                                     ,(format-timestring *validity-end* :timezone +utc-zone+)
                                   (bind ((persistent-value (slot-value instance ',slot-name))
                                          (test-value (slot-value* instance ',slot-name)))
                                     (assert-persistent-and-test-values instance ',slot-name persistent-value test-value))))))))))
            (return-from test/tesites/complex))
           :report-function (lambda (stream)
                              (format stream "Print a specific test case for this error and skip this complex test"))))
      (handler-bind
          ((serious-condition
            (lambda (e)
              (setf error e))))
        (iter (repeat repeat-count)
              (with-transaction
                (do-random-operations instances
                  :count operation-count
                  :slot-names (if slot-name
                                  (list slot-name)
                                  slot-names)))
              (finally
               (when full-test
                 (full-compare-history instances))
               ;; default x default
               (with-transaction
                 (with-default-t
                   (compare-history instances)))
               (iter (repeat test-count)
                     ;; default x random
                     (with-transaction
                       (with-default-t
                         (with-random-validity-range
                           (compare-history instances))))
                     ;; random x default
                     (with-transaction
                       (with-random-t
                         (compare-history instances)))
                     ;; random x random
                     (with-transaction
                       (with-random-t
                         (with-random-validity-range
                           (compare-history instances)))))))))))
