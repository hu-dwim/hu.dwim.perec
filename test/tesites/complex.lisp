;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;
;;; Complex

(defvar *history-entry-counter*)

(defvar *history-entries*)

(defvar *transaction-counter*)

(defstruct (history-entry (:conc-name he-))
  (step (incf *history-entry-counter*) :type integer)
  (transaction-index *transaction-counter* :type integer)
  (action nil :type (member :set :insert :delete))
  (instance nil :type (or null persistent-object))
  (slot-name nil :type symbol)
  (t-value nil :type (or null local-time))
  (validity-start nil :type (or null local-time))
  (validity-end nil :type (or null local-time))
  (value nil :type t))

(defun sort-entries-by-step (entries &key (ascending #t))
  (sort entries (if ascending #'< #'>) :key #'he-step))

(defun sort-entries-by-t (entries &key (ascending #t))
  (bind ((local-time-comparator (if ascending #'local-time< #'local-time>))
         (step-comparator (if ascending #'< #'>)))
    (sort entries (lambda (value-1 value-2)
                    (or (funcall local-time-comparator
                                 (he-t-value value-1)
                                 (he-t-value value-2))
                        (and (local-time= (he-t-value value-1)
                                          (he-t-value value-2))
                             (funcall step-comparator
                                      (he-step value-1)
                                      (he-step value-2))))))))

(defun slot-value* (instance slot-name)
  (bind ((class (class-of instance))
         (slot (find-slot class slot-name))
         (association-slot-p (typep slot 'persistent-association-end-effective-slot-definition))
         (other-slot (when association-slot-p (prc::other-association-end-of slot)))
         (other-slot-name (when association-slot-p (slot-definition-name other-slot)))
         (association-kind (when association-slot-p (prc::association-kind-of (prc::association-of slot))))
         (temporal-p (and (typep slot 'persistent-effective-slot-definition-t) (prc::temporal-p slot)))
         (time-dependent-p (and (typep slot 'persistent-effective-slot-definition-t) (prc::time-dependent-p slot)))
         ((:values slot-default-value has-default-p) (prc::default-value-for-type (prc::canonical-type-of slot)))
         (default-value (if has-default-p
                            slot-default-value
                            +unbound-slot-marker+))
         (sort-function (if temporal-p
                            #'sort-entries-by-t
                            #'sort-entries-by-step))
         (temporal-filter-function (if temporal-p
                                       #L(local-time<= (he-t-value !1) *t*)
                                       (constantly #t)))
         (time-dependent-filter-function (if time-dependent-p
                                             #L(and (local-time< (he-validity-start !1) *validity-end*)
                                                    (local-time< *validity-start* (he-validity-end !1)))
                                             (constantly #t)))
         (filter-function #L(and (funcall temporal-filter-function !1)
                                 (funcall time-dependent-filter-function !1)))
         (current-slot-value-function (if time-dependent-p
                                          #'values-having-validity-value
                                          (lambda (current-value validity-start validity-end)
                                            (declare (ignore validity-start validity-end))
                                            current-value)))
         (new-slot-value-function (if time-dependent-p
                                      (lambda (new-value old-value validity-start validity-end)
                                        (if (values-having-validity-p old-value)
                                            (progn
                                              (setf (values-having-validity-value old-value validity-start validity-end) new-value)
                                              old-value)
                                            (make-single-values-having-validity new-value validity-start validity-end)))
                                      (lambda (new-value old-value validity-start validity-end)
                                        (declare (ignore old-value validity-start validity-end))
                                        new-value)))
         (return-value-function (if time-dependent-p
                                    #L(collect-values-having-validity !1 #'he-value #'he-validity-start #'he-validity-end (constantly default-value) *validity-start* *validity-end*)
                                    #L(aif (first !1)
                                           (he-value it)
                                           default-value)))

         (value
          (if association-slot-p
              (iter (with slot-value = default-value)
                    (for entry :in (funcall sort-function (collect-if filter-function *history-entries*) :ascending #t))
                    (for he-action = (he-action entry))
                    (for he-value = (he-value entry))
                    (for he-instance = (he-instance entry))
                    (for he-slot-name = (he-slot-name entry))
                    (labels ((%slot-value ()
                               (funcall current-slot-value-function slot-value (he-validity-start entry) (he-validity-end entry)))
                             ((setf %slot-value) (new-value)
                               (setf slot-value (funcall new-slot-value-function new-value slot-value (he-validity-start entry) (he-validity-end entry))))
                             (%push-slot-value (new-value)
                               (setf (%slot-value) (cons new-value (%slot-value))))
                             (%remove-slot-value (new-value)
                               (setf (%slot-value) (remove new-value (%slot-value) :test #'p-eq))))
                      (cond ((eq slot-name he-slot-name)
                             (if (p-eq instance he-instance)
                                 (ecase he-action
                                   (:set (setf (%slot-value) he-value))
                                   (:insert (%push-slot-value he-value))
                                   (:delete (%remove-slot-value he-value)))
                                 (ecase association-kind
                                   (:1-1 (when (p-eq slot-value he-value)
                                           (setf (%slot-value) default-value)))
                                   (:1-n (when (eq :n (prc::cardinality-kind-of slot))
                                           (ecase he-action
                                             (:set (setf (%slot-value) (set-difference (%slot-value) he-value)))
                                             (:insert (%remove-slot-value he-value))
                                             (:delete (values)))))
                                   (:m-n (values)))))
                            ((eq other-slot-name he-slot-name)
                             (ecase association-kind 
                               (:1-1 (cond ((p-eq instance he-value)
                                            (setf (%slot-value) he-instance))
                                           ((and (not (p-eq instance he-value))
                                                 (p-eq slot-value he-instance))
                                            (setf (%slot-value) default-value))))
                               (:1-n (ecase (prc::cardinality-kind-of slot)
                                       (:1 (bind ((he-instance-is-slot-value (p-eq slot-value he-instance)))
                                             (ecase he-action
                                               (:set (bind ((instance-is-member-of-he-value (member instance he-value :test #'p-eq)))
                                                       (cond ((and he-instance-is-slot-value
                                                                   (not instance-is-member-of-he-value))
                                                              (setf (%slot-value) default-value))
                                                             ((and (not he-instance-is-slot-value)
                                                                   instance-is-member-of-he-value)
                                                              (setf (%slot-value) he-instance)))))
                                               (:insert (bind ((instance-is-he-value (p-eq instance he-value)))
                                                          (when (and (not he-instance-is-slot-value)
                                                                     instance-is-he-value)
                                                            (setf (%slot-value) he-instance))))
                                               (:delete (bind ((instance-is-he-value (p-eq instance he-value)))
                                                          (when (and he-instance-is-slot-value
                                                                     instance-is-he-value)
                                                            (setf (%slot-value) default-value)))))))
                                       (:n (bind ((he-instance-is-member-of-slot-value (member he-instance slot-value :test #'p-eq))
                                                  (instance-is-he-value (p-eq instance he-value)))
                                             (cond ((and he-instance-is-member-of-slot-value
                                                         (not instance-is-he-value))
                                                    (%remove-slot-value he-instance))
                                                   ((and (not he-instance-is-member-of-slot-value)
                                                         instance-is-he-value)
                                                    (%push-slot-value he-instance)))))))
                               (:m-n (bind ((he-instance-is-member-of-slot-value (member he-instance slot-value :test #'p-eq)))
                                       (ecase he-action
                                         (:set (bind ((instance-is-member-of-he-value (member instance he-value :test #'p-eq)))
                                                 (cond ((and he-instance-is-member-of-slot-value
                                                             (not instance-is-member-of-he-value))
                                                        (%remove-slot-value he-instance))
                                                       ((and (not he-instance-is-member-of-slot-value)
                                                             instance-is-member-of-he-value)
                                                        (%push-slot-value he-instance)))))
                                         (:insert (bind ((instance-is-he-value (p-eq instance he-value)))
                                                    (when (and (not he-instance-is-member-of-slot-value)
                                                               instance-is-he-value)
                                                      (%push-slot-value he-instance))))
                                         (:delete (bind ((instance-is-he-value (p-eq instance he-value)))
                                                    (when (and he-instance-is-member-of-slot-value
                                                               instance-is-he-value)
                                                      (%remove-slot-value he-instance)))))))))))
                    (finally (return slot-value)))
              (funcall return-value-function
                       (collect-if filter-function
                                   (funcall sort-function
                                            (collect-if #L(and (eq slot-name (he-slot-name !1))
                                                               (p-eq instance (he-instance !1)))
                                                        *history-entries*)
                                            :ascending #f))))))
    (cond ((single-values-having-validity-p value)
           (first-elt (prc::values-of value)))
          ((and (values-having-validity-p value)
                (iter (for (s e v) :in-values-having-validity value)
                      (thereis (unbound-slot-marker-p v))))
           +unbound-slot-marker+)
          (t value))))

(defun (setf slot-value*) (new-value instance slot-name)
  (assert (not (values-having-validity-p new-value)))
  (push (make-history-entry :action :set
                            :instance instance
                            :slot-name slot-name
                            :t-value *t*
                            :validity-start *validity-start*
                            :validity-end *validity-end*
                            :value new-value)
        *history-entries*))

(defun (setf slot-value-and-slot-value*) (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value)
  (setf (slot-value* instance slot-name) new-value))

(defun insert-item-and-insert-item* (instance slot-name item)
  (with-lazy-slot-value-collections
    (insert-item (slot-value instance slot-name) item)
    (push (make-history-entry :action :insert
                              :instance instance
                              :slot-name slot-name
                              :t-value *t*
                              :validity-start *validity-start*
                              :validity-end *validity-end*
                              :value item)
          *history-entries*)))

(defun delete-item-and-delete-item* (instance slot-name item)
  (with-lazy-slot-value-collections
    (delete-item (slot-value instance slot-name) item)
    (push (make-history-entry :action :delete
                              :instance instance
                              :slot-name slot-name
                              :t-value *t*
                              :validity-start *validity-start*
                              :validity-end *validity-end*
                              :value item)
          *history-entries*)))

(defun complex-test-slot-names (class slot-names)
  (bind ((available-slot-names
          (iter (for slot :in (prc::persistent-effective-slots-of class))
                (for slot-name = (slot-definition-name slot))
                (unless (starts-with-subseq (symbol-name slot-name) "H-") ;was (eq slot-name 'h-objects)
                  (collect slot-name)))))
    (if slot-names
        (intersection slot-names available-slot-names)
        available-slot-names)))

(defun generate-instances (class-names count &key (slot-names nil))
  (iter (repeat count)
        (for class-name = (random-elt class-names))
        (for instance = (make-instance* class-name :slot-names slot-names))
        (format t "Generated instance ~A~%" instance)
        (collect instance)))

(defun make-instance* (class-name &key (slot-names nil))
  (iter (with instance = (make-instance class-name))
        (for slot-name :in (complex-test-slot-names (class-of instance) slot-names))
        (setf (slot-value* instance slot-name)
              (if (slot-boundp instance slot-name)
                  (slot-value instance slot-name)
                  +unbound-slot-marker+))
        (finally (return instance))))

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
             (iter (for (persistent-validity-start persistent-validity-end persistent-value) :in-values-having-validity (consolidate-values-having-validity persistent-value))
                   (for (test-validity-start test-validity-end test-value) :in-values-having-validity (consolidate-values-having-validity test-value))
                   (always (and (compare persistent-value test-value)
                                (local-time= persistent-validity-start test-validity-start)
                                (local-time= persistent-validity-end test-validity-end)))))
        (compare persistent-value test-value))))

(defun assert-persistent-and-test-values (instance slot-name persistent-value test-value)
  (is (compare-persistent-and-test-values persistent-value test-value)
      "The persistent value: ~A and test value: ~A are different~%in the slot ~A of ~A~%with t ~A and with validity range ~A -> ~A~%with ~A history entries: ~A"
      persistent-value test-value slot-name instance *t* *validity-start* *validity-end* (length *history-entries*) *history-entries*))

(defun compare-history (instances &key (slot-names nil))
  (iter (for instance :in instances)
        (for class = (class-of instance))
        (revive-instance instance)
        (iter (for slot-name :in (complex-test-slot-names (class-of instance) slot-names))
              (for persistent-value = (if (slot-boundp instance slot-name)
                                          (slot-value instance slot-name)
                                          +unbound-slot-marker+))
              (for test-value = (slot-value* instance slot-name))
              (assert-persistent-and-test-values instance slot-name persistent-value test-value))))

(defun full-compare-history (instances &key (slot-names nil) (add-epsilon-timestamps t))
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
                              (compare-history instances :slot-names slot-names))))))))))

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

(defun do-random-operation (instances &key (slot-names nil))
  (bind ((instance (load-instance (random-elt instances)))
         (slot-name (random-elt (complex-test-slot-names (class-of instance) slot-names)))
         (slot-type (slot-definition-type (find-slot (class-of instance) slot-name)))
         ((:values action slot-value)
          (cond ((primitive-type-p* slot-type)
                 (values :set (random 100)))
                ((persistent-class-type-p* slot-type)
                 (values :set (random-elt (cons nil (collect-if #L(typep !1 slot-type) instances)))))
                ((set-type-p* slot-type)
                 (bind ((instances (collect-if #L(typep !1 (prc::set-type-class-for slot-type)) instances)))
                   (ecase (random 3)
                     (0 (values :set (subseq (shuffle instances) 0 (random (length instances)))))
                     (1 (values :insert (random-elt instances)))
                     (2 (values :delete (random-elt instances))))))
                (t (error "Unknown type ~A" slot-type)))))
    (format t "+ ~A slot ~A of ~A~%  with t ~A and with validity range ~A -> ~A~%  the value ~A~%"
            (ecase action
              (:set "Setting")
              (:insert "Inserting into")
              (:delete "Deleting from"))
            slot-name instance *t* *validity-start* *validity-end* slot-value)
    (ecase action
      (:set (setf (slot-value-and-slot-value* instance slot-name) slot-value))
      (:insert (insert-item-and-insert-item* instance slot-name slot-value))
      (:delete (delete-item-and-delete-item* instance slot-name slot-value)))))

(defun run-complex-test (&key (class-name nil) (class-names (when class-name (list class-name))) (instance-count 1) (operation-count 1) (transaction-count 1) (timestamp-count 10)
                         (full-test #t) (test-epsilon-timestamps #t) (random-test-count 1) (slot-name nil) (slot-names (when slot-name (list slot-name))))
  (bind ((*history-entries* nil)
         (*history-entry-counter* 0)
         (*transaction-counter* 0)
         (error nil)
         (instances
          (with-transaction
            (with-default-t
              (generate-instances class-names instance-count :slot-names slot-names)))))
    (format t "Starting operations with ~A number of history entries...~%" (length *history-entries*))
    (restart-bind
        ((print-test
          (lambda ()
            (flet ((instance-variable-name (instance)
                     (concatenate-symbol "instance-" (prc::id-of instance))))
              (bind ((*print-level* nil)
                     (*print-length* nil)
                     (*print-lines* nil)
                     (*print-circle* #f)
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
                                  (*transaction-counter* 0)
                                  ,@(iter (for instance :in instances)
                                          (collect `(,(instance-variable-name instance)
                                                      (with-transaction
                                                        (with-default-t
                                                          (make-instance ',(class-name (class-of instance)))))))))
                             ,@(iter (for transaction-counter :from 0 :to *transaction-counter*)
                                     (for history-entries = (collect-if #L(= transaction-counter (he-transaction-index !1)) *history-entries*))
                                     (when history-entries
                                       (appending
                                        `((with-transaction
                                            (with-revived-instances ,(mapcar #'instance-variable-name instances)
                                              ,@(iter (for entry :in history-entries)
                                                      (for value = (bind ((value (he-value entry)))
                                                                     (cond ((typep value 'persistent-object)
                                                                            (instance-variable-name value))
                                                                           ((listp value)
                                                                            `(list ,@(mapcar #'instance-variable-name value)))
                                                                           (t value))))
                                                      (for instance = (instance-variable-name (he-instance entry)))
                                                      (for slot-name = (he-slot-name entry))
                                                      (collect `(with-t ,(format-timestring (he-t-value entry) :timezone +utc-zone+)
                                                                  (with-validity-range
                                                                      ,(format-timestring (he-validity-start entry) :timezone +utc-zone+)
                                                                      ,(format-timestring (he-validity-end entry) :timezone +utc-zone+)
                                                                    ,(ecase (he-action entry)
                                                                            (:set `(setf (slot-value-and-slot-value* ,instance ',slot-name) ,value))
                                                                            (:insert `(insert-item-and-insert-item* ,instance ',slot-name ,value))
                                                                            (:delete `(delete-item-and-delete-item* ,instance ',slot-name ,value)))))))))
                                          (incf *transaction-counter*)))))
                             (setf *history-entries* (nreverse *history-entries*))
                             (with-transaction
                               (with-revived-instance ,instance-variable-name
                                 (with-t ,(format-timestring *t* :timezone +utc-zone+)
                                   (with-validity-range
                                       ,(format-timestring *validity-start* :timezone +utc-zone+)
                                       ,(format-timestring *validity-end* :timezone +utc-zone+)
                                     (bind ((persistent-value (if (slot-boundp ,instance-variable-name ',slot-name)
                                                                  (slot-value ,instance-variable-name ',slot-name)
                                                                  +unbound-slot-marker+))
                                            (test-value (slot-value* ,instance-variable-name ',slot-name)))
                                       (assert-persistent-and-test-values ,instance-variable-name ',slot-name persistent-value test-value))))))))))))
           :report-function (lambda (stream)
                              (format stream "Print a specific test case for this error"))))
      (handler-bind
          ((serious-condition
            (lambda (e)
              (setf error e))))
        (iter (with timestamps = (coerce (iter (repeat timestamp-count)
                                               (when (first-iteration-p)
                                                 (collect +beginning-of-time+)
                                                 (collect +end-of-time+))
                                               (collect (random-local-time)))
                                         'simple-vector))
              (repeat transaction-count)
              (flet ((random-timestamp (&optional (offset 0))
                       (bind ((max (- (length timestamps) offset))
                              (index (+ offset (random max))))
                         (values (aref timestamps index) index))))
                (with-transaction
                  (format t "Starting new transaction~%")
                  (iter (repeat operation-count)
                        (with-t (random-elt timestamps)
                          (bind (((:values start end)
                                  (iter (for start = (random-elt timestamps))
                                        (for end = (random-elt timestamps))
                                        (until (local-time> end start))
                                        (finally (return (values start end))))))
                            (with-validity-range start end
                              (do-random-operation instances :slot-names slot-names))))))
                (incf *transaction-counter*))
              (finally
               (setf *history-entries* (nreverse *history-entries*))
               (when full-test
                 (full-compare-history instances :slot-names slot-names :add-epsilon-timestamps test-epsilon-timestamps))
               ;; default x default
               (with-transaction
                 (with-default-t
                   (compare-history instances :slot-names slot-names)))
               (iter (repeat random-test-count)
                     ;; default x random
                     (with-transaction
                       (with-default-t
                         (with-random-validity-range
                           (compare-history instances :slot-names slot-names))))
                     ;; random x default
                     (with-transaction
                       (with-random-t
                         (compare-history instances :slot-names slot-names)))
                     ;; random x random
                     (with-transaction
                       (with-random-t
                         (with-random-validity-range
                           (compare-history instances :slot-names slot-names)))))))))))

(deftest run-complex-tests (&rest args &key (instance-count 10) (operation-count 10) &allow-other-keys)
  (apply #'run-complex-test
         :instance-count instance-count
         :operation-count 1
         :transaction-count 1
         :random-test-count 1
         args)
  (apply #'run-complex-test
         :instance-count instance-count
         :operation-count operation-count
         :transaction-count 10
         :random-test-count 10
         :test-epsilon-timestamps #f
         args))
