;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;
;;; Complex

(defsuite* (test/tesites/complex :in test/tesites))

(defpclass* tesites-complex-test ()
  ((slot 0 :type integer-32)
   (temporal-slot 0 :type integer-32 :temporal #t)
   (time-dependent-slot 0 :type integer-32 :time-dependent #t)
   (temporal-and-time-dependent-slot 0 :type integer-32 :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-unbound-test ()
  ((slot :type (or unbound integer-32))
   (temporal-slot :type (or unbound integer-32) :temporal #t)
   (time-dependent-slot :type (or unbound integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or unbound integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-null-test ()
  ((slot :type (or null integer-32))
   (temporal-slot :type (or null integer-32) :temporal #t)
   (time-dependent-slot :type (or null integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or null integer-32) :temporal #t :time-dependent #t)))

(defpclass* tesites-complex-unbound-or-null-test ()
  ((slot :type (or unbound null integer-32))
   (temporal-slot :type (or unbound null integer-32) :temporal #t)
   (time-dependent-slot :type (or unbound null integer-32) :time-dependent #t)
   (temporal-and-time-dependent-slot :type (or unbound null integer-32) :temporal #t :time-dependent #t)))

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

(defun sort-entries-by-step (entries &key (ascending #t))
  (stable-sort entries (if ascending #'< #'>) :key #'he-step))

(defun sort-entries-by-t (entries &key (ascending #t))
  (stable-sort entries (if ascending #'local-time< #'local-time>) :key #'he-t-value))

(defun matches-entry-instance-p (entry instance slot-name)
  (and (eq slot-name (he-slot-name entry))
       (p-eq instance (he-instance entry))))

(defun validity-range-overlaps-p (entry)
  (and (local-time< (he-validity-start entry) *validity-end*)
       (local-time< *validity-start* (he-validity-end entry))))

(defun slot-value* (instance slot-name)
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name))
         (association-slot-p (typep slot 'persistent-association-end-effective-slot-definition))
         (other-slot (when association-slot-p (prc::other-association-end-of slot)))
         (other-slot-name (when association-slot-p (slot-definition-name other-slot)))
         (association-kind (when association-slot-p (prc::association-kind-of (prc::association-of slot))))
         (temporal-p (and (typep slot 'persistent-effective-slot-definition-t) (prc::temporal-p slot)))
         (time-dependent-p (and (typep slot 'persistent-effective-slot-definition-t) (prc::time-dependent-p slot)))
         (history-entries (collect-if #L(matches-entry-instance-p !1 instance slot-name) *history-entries*))
         ((:values slot-default-value has-default-p) (prc::default-value-for-type (prc::canonical-type-of slot)))
         (default-value (if has-default-p slot-default-value +unbound-slot-marker+))
         (value
          (cond ((and (not temporal-p)
                      (not time-dependent-p))
                 (if association-slot-p
                     (iter (with slot-value = default-value)
                           (for entry :in (sort-entries-by-step (copy-seq *history-entries*) :ascending #t))
                           (cond ((eq slot-name (he-slot-name entry))
                                  (if (p-eq instance (he-instance entry))
                                      (setf slot-value (he-value entry))
                                      (ecase association-kind
                                        (:1-1 (when (p-eq slot-value (he-value entry))
                                                (setf slot-value default-value)))
                                        (:1-n (ecase (prc::cardinality-kind-of slot)
                                                (:1 (when (p-eq slot-value (he-value entry))
                                                      (setf slot-value default-value)))
                                                (:n (setf slot-value (set-difference slot-value (he-value entry))))))
                                        (:m-n (values)))))
                                 ((eq other-slot-name (he-slot-name entry))
                                  (ecase association-kind 
                                    (:1-1 (cond ((p-eq instance (he-value entry))
                                                 (setf slot-value (he-instance entry)))
                                                ((and (not (p-eq instance (he-value entry)))
                                                      (p-eq slot-value (he-instance entry)))
                                                 (setf slot-value default-value))))
                                    (:1-n (ecase (prc::cardinality-kind-of slot)
                                            (:1 (bind ((he-instance-is-slot-value (p-eq slot-value (he-instance entry)))
                                                       (instance-is-member-of-he-value (member instance (he-value entry) :test #'p-eq)))
                                                  (cond ((and he-instance-is-slot-value
                                                              (not instance-is-member-of-he-value))
                                                         (setf slot-value default-value))
                                                        ((and (not he-instance-is-slot-value)
                                                              instance-is-member-of-he-value)
                                                         (setf slot-value (he-instance entry))))))
                                            (:n (bind ((he-instance-is-member-of-slot-value (member (he-instance entry) slot-value :test #'p-eq))
                                                       (instance-is-he-value (p-eq instance (he-value entry))))
                                                  (cond ((and he-instance-is-member-of-slot-value
                                                              (not instance-is-he-value))
                                                         (alexandria:removef slot-value (he-instance entry) :test #'p-eq))
                                                        ((and (not he-instance-is-member-of-slot-value)
                                                              instance-is-he-value)
                                                         (push (he-instance entry) slot-value)))))))
                                    (:m-n (bind ((he-instance-is-member-of-slot-value (member (he-instance entry) slot-value :test #'p-eq))
                                                 (instance-is-member-of-he-value (member instance (he-value entry) :test #'p-eq)))
                                            (cond ((and he-instance-is-member-of-slot-value
                                                        (not instance-is-member-of-he-value))
                                                   (alexandria:removef slot-value (he-instance entry) :test #'p-eq))
                                                  ((and (not he-instance-is-member-of-slot-value)
                                                        instance-is-member-of-he-value)
                                                   (push (he-instance entry) slot-value))))))))
                           (finally (return slot-value)))
                     (aif (first (sort-entries-by-step history-entries :ascending #f))
                          (he-value it)
                          default-value)))
                ((and temporal-p
                      time-dependent-p)
                 (prc::collect-values-having-validity
                  (sort-entries-by-t
                   (collect-if (lambda (entry)
                                 (and (local-time<= (he-t-value entry) *t*)
                                      (validity-range-overlaps-p entry)))
                               history-entries)
                   :ascending #f)
                  #'he-value #'he-validity-start #'he-validity-end (constantly default-value) *validity-start* *validity-end*))
                (temporal-p
                 (aif (find-if #L(local-time<= (he-t-value !1) *t*)
                               (sort-entries-by-t history-entries :ascending #f))
                      (he-value it)
                      default-value))
                (time-dependent-p
                 (prc::collect-values-having-validity
                  (collect-if #'validity-range-overlaps-p history-entries)
                  #'he-value #'he-validity-start #'he-validity-end (constantly default-value) *validity-start* *validity-end*)))))
    (cond ((single-values-having-validity-p value)
           (elt-0 (prc::values-of value)))
          ((and (values-having-validity-p value)
                (iter (for (v s e) :in-values-having-validity value)
                      (thereis (prc::unbound-slot-marker-p v))))
           +unbound-slot-marker+)
          (t value))))

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

(defun complext-test-slot-names (class)
  (iter (for slot :in (prc::persistent-effective-slots-of class))
        (for slot-name = (slot-definition-name slot))
        (unless (eq slot-name 'h-objects)
          (collect slot-name))))

(defun generate-instances (class-names count)
  (iter (repeat count)
        (for class-name = (random-elt class-names))
        (for instance = (make-instance class-name))
        (iter (for slot-name :in (complext-test-slot-names (class-of instance)))
              (when (slot-boundp instance slot-name)
                (setf (slot-value* instance slot-name) (slot-value instance slot-name))))
        (format t "Generated instance ~A~%" instance)
        (collect instance)))

(defun compare-persistent-and-test-values (persistent-value test-value)
  (flet ((compare (persistent-value test-value)
           (cond ((and (typep persistent-value 'persistent-object)
                       (typep test-value 'persistent-object))
                  (p-eq persistent-value test-value))
                 ((and (listp persistent-value)
                       (listp test-value))
                  (every #'p-eq
                         (sort (copy-seq persistent-value) #'< :key #'prc::id-of)
                         (sort (copy-seq test-value) #'< :key #'prc::id-of)))
                 (t (eql persistent-value test-value)))))
    (or (and (values-having-validity-p persistent-value)
             (values-having-validity-p test-value)
             (iter (for (persistent-value persistent-validity-start persistent-validity-end) :in-values-having-validity persistent-value)
                   (for (test-value test-validity-start test-validity-end) :in-values-having-validity test-value)
                   (always (and (compare persistent-value test-value)
                                (local-time= persistent-validity-start test-validity-start)
                                (local-time= persistent-validity-end test-validity-end)))))
        (compare persistent-value test-value))))

(defun assert-persistent-and-test-values (instance slot-name persistent-value test-value)
  (is (compare-persistent-and-test-values persistent-value test-value)
      "The persistent value: ~A and test value: ~A are different~%in the slot ~A of ~A~%with t ~A and with validity range ~A -> ~A~%with ~A history entries: ~A"
      persistent-value test-value slot-name instance *t* *validity-start* *validity-end* (length *history-entries*) *history-entries*))

(defun compare-history (instances)
  (iter (for instance :in instances)
        (for class = (class-of instance))
        (revive-instance instance)
        (iter (for slot-name :in (complext-test-slot-names class))
              (for persistent-value = (if (slot-boundp instance slot-name)
                                          (slot-value instance slot-name)
                                          +unbound-slot-marker+))
              (for test-value = (slot-value* instance slot-name))
              (assert-persistent-and-test-values instance slot-name persistent-value test-value))))

(defun full-compare-history (instances &key (add-epsilon-timestamps t))
  (labels ((extend-timestamps (timestamps)
             (bind ((epsilon-nsec 1000000))
               (append (list +beginning-of-time+)
                       (if add-epsilon-timestamps
                           (mapcan #L(list (adjust-local-time !1 (offset :nsec (- epsilon-nsec)))
                                           !1
                                           (adjust-local-time !1 (offset :nsec epsilon-nsec)))
                                   timestamps)
                           timestamps)
                       (list +end-of-time+))))
           (fixup-timestamps (timestamps)
             (sort (delete-duplicates (delete-if #L(or (local-time< !1 +beginning-of-time+)
                                                       (local-time< +end-of-time+ !1))
                                                 (extend-timestamps timestamps))
                                      :test #'local-time=)
                   #'local-time<)))
    (bind ((t-values (fixup-timestamps
                      (mapcar 'he-t-value *history-entries*)))
           (validity-values (fixup-timestamps
                             (append (mapcar 'he-validity-start *history-entries*)
                                     (mapcar 'he-validity-end *history-entries*)))))
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
                              (compare-history instances))))))))))

(defun random-elt (sequence)
  (elt sequence (random (length sequence))))

(defun random-universal-time ()
  (random 5000000000))

(defun random-local-time ()
  (local-time :universal (random-universal-time) :timezone +utc-zone+))

(defmacro with-random-t (&body forms)
  `(with-t (random-local-time)
     ,@forms))

(defmacro with-random-validity-range (&body forms)
  `(bind ((validity-start (random-local-time))
          (validity-end (local-time+ validity-start (random-universal-time))))
     (with-validity-range validity-start validity-end
       ,@forms)))

(defun do-random-operations (instances &key (count 1) (slot-names nil))
  (iter (repeat count)
        (bind ((instance (load-instance (random-elt instances)))
               (slot-names (or slot-names (complext-test-slot-names (class-of instance))))
               (slot-name (random-elt slot-names))
               (slot-type (slot-definition-type (find-slot (class-of instance) slot-name)))
               (value (cond ((primitive-type-p* slot-type)
                             (random 100))
                            ((persistent-class-type-p* slot-type)
                             (random-elt (cons nil (collect-if #L(typep !1 slot-type) instances))))
                            ((set-type-p* slot-type)
                             (bind ((instances (alexandria:shuffle (collect-if #L(typep !1 (prc::set-type-class-for slot-type)) instances))))
                               (subseq instances 0 (min (length instances) (random 4)))))
                            (t (error "Unknown type ~A" slot-type)))))
          (format t "Setting slot ~A of ~A~% with t ~A and with validity range ~A -> ~A~%to ~A~%"
                  slot-name instance *t* *validity-start* *validity-end* value)
          (setf (slot-value-and-slot-value* instance slot-name) value))))

(defun run-complex-test (&key (class-name nil) (class-names nil) (instance-count 1) (operation-count 1) (repeat-count 1) (new-timestamp-probability 0.25)
                         (full-test #t) (test-epsilon-timestamps #t) (random-test-count 1) (slot-name nil) (slot-names nil))
  (bind ((*history-entries* nil)
         (*history-entry-counter* 0)
         (error nil)
         (instances
          (with-transaction
            (with-default-t
              (generate-instances (if class-name
                                      (list class-name)
                                      class-names)
                                  instance-count)))))
    (format t "Starting operations with ~A number of history entries...~%" (length *history-entries*))
    (restart-bind
        ((print-test
          (lambda ()
            (flet ((instance-variable-name (instance)
                     (concatenate-symbol "instance-" (prc::id-of instance))))
              (bind ((*print-level* nil)
                     (*print-length* nil)
                     (*print-lines* nil)
                     (failure-descriptions (stefil::failure-descriptions-of stefil::*global-context*))
                     (failure-description (aref failure-descriptions (1- (length failure-descriptions))))
                     (format-arguments (stefil::format-arguments-of failure-description))
                     (slot-name (elt format-arguments 2))
                     (instance (elt format-arguments 3))
                     (instance-variable-name (instance-variable-name instance)))
                (format t "~%~S"
                        `(deftest test/tesites/complex/generated ()
                           ,(format nil "~A" error)
                           (bind ((*history-entries* nil)
                                  (*history-entry-counter* 0)
                                  ,@(iter (for instance :in instances)
                                          (collect `(,(instance-variable-name instance)
                                                      (with-transaction
                                                        (with-default-t
                                                          (make-instance ',(class-name (class-of instance)))))))))
                             (with-transaction
                               (with-revived-instances ,(mapcar #'instance-variable-name instances)
                                 ,@(iter (for entry :in (reverse *history-entries*))
                                         (collect `(with-t ,(format-timestring (he-t-value entry) :timezone +utc-zone+)
                                                     (with-validity-range
                                                         ,(format-timestring (he-validity-start entry) :timezone +utc-zone+)
                                                         ,(format-timestring (he-validity-end entry) :timezone +utc-zone+)
                                                       (setf (slot-value-and-slot-value* ,(instance-variable-name (he-instance entry)) ',(he-slot-name entry))
                                                             ,(bind ((value (he-value entry)))
                                                                    (cond ((typep value 'persistent-object)
                                                                           (instance-variable-name value))
                                                                          ((listp value)
                                                                           `(list ,@(mapcar #'instance-variable-name value)))
                                                                          (t value))))))))))
                             (with-transaction
                               (with-revived-instance ,instance-variable-name
                                 (with-t ,(format-timestring *t* :timezone +utc-zone+)
                                   (with-validity-range
                                       ,(format-timestring *validity-start* :timezone +utc-zone+)
                                       ,(format-timestring *validity-end* :timezone +utc-zone+)
                                     (bind ((persistent-value (slot-value ,instance-variable-name ',slot-name))
                                            (test-value (slot-value* ,instance-variable-name ',slot-name)))
                                       (assert-persistent-and-test-values ,instance-variable-name ',slot-name persistent-value test-value)))))))))))
            (return-from run-complex-test))
           :report-function (lambda (stream)
                              (format stream "Print a specific test case for this error and skip this complex test"))))
      (handler-bind
          ((serious-condition
            (lambda (e)
              (setf error e))))
        (iter (with timestamps = (make-array 0 :fill-pointer 0))
              (repeat repeat-count)
              (flet ((random-timestamp (&optional (offset 0))
                       (bind ((max (- (length timestamps) offset)))
                         (if (or (<= max 0)
                                 (< (random 1.0) new-timestamp-probability))
                             (bind ((timestamp (random-local-time)))
                               (vector-push-extend timestamp timestamps)
                               (values timestamp 0))
                             (bind ((index (+ offset (random max))))
                               (values (aref timestamps index) index))))))
                (with-transaction
                  (with-t (random-timestamp)
                    (bind (((:values start index) (random-timestamp))
                           (end (iter (for end = (random-timestamp index))
                                      (until (local-time> end start))
                                      (finally (return end)))))
                      (with-validity-range start end
                        (do-random-operations instances
                          :count operation-count
                          :slot-names (if slot-name
                                          (list slot-name)
                                          slot-names)))))))
              (finally
               (when full-test
                 (full-compare-history instances :add-epsilon-timestamps test-epsilon-timestamps))
               ;; default x default
               (with-transaction
                 (with-default-t
                   (compare-history instances)))
               (iter (repeat random-test-count)
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

(deftest test/tesites/complex/slot ()
  (run-complex-test :class-name 'tesites-complex-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/complex/unbound-slot ()
  (run-complex-test :class-name 'tesites-complex-unbound-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/complex/null-slot ()
  (run-complex-test :class-name 'tesites-complex-null-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/complex/unbound-or-null-slot ()
  (run-complex-test :class-name 'tesites-complex-unbound-or-null-test
                    :instance-count 1
                    :operation-count 10))

(deftest test/tesites/complex/inheritance ()
  (run-complex-test :class-name 'tesites-complex-inheritance-test
                    :instance-count 1
                    :operation-count 10))
