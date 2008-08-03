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
  (t-value nil :type (or null timestamp))
  (validity-start nil :type (or null timestamp))
  (validity-end nil :type (or null timestamp))
  (value nil :type t))

(defun sort-entries-by-step (entries &key (ascending #t))
  (sort entries (if ascending #'< #'>) :key #'he-step))

(defun sort-entries-by-t (entries &key (ascending #t))
  (bind ((local-time-comparator (if ascending #'timestamp< #'timestamp>))
         (step-comparator (if ascending #'< #'>)))
    (sort entries (lambda (value-1 value-2)
                    (or (funcall local-time-comparator
                                 (he-t-value value-1)
                                 (he-t-value value-2))
                        (and (timestamp= (he-t-value value-1)
                                         (he-t-value value-2))
                             (funcall step-comparator
                                      (he-step value-1)
                                      (he-step value-2))))))))

(defun take (n list)
  (labels ((recurse (n list result)
             (cond
               ((zerop n) result)
               ((null list) result)
               (t (recurse (1- n) (rest list) (cons (first list) result))))))
    (nreverse (recurse n list nil))))

(defgeneric filter-and-sort-history-entries (instance slot)

  ;; normal slot
  (:method ((instance persistent-object) (slot persistent-slot-definition))
    (bind ((slot-name (slot-definition-name slot)))
      (take 1
            (sort-entries-by-step
             (collect-if #L(and (eq slot-name (he-slot-name !1))
                                (p-eq instance (he-instance !1)))
                         *history-entries*)
             :ascending #f))))

  ;; normal association-end
  (:method ((instance persistent-object) (slot persistent-association-end-slot-definition))
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot))))
      (sort-entries-by-step
       (collect-if #L(or (eq slot-name (he-slot-name !1))
                         (eq other-slot-name (he-slot-name !1)))
                   *history-entries*)
       :ascending #t)))

  ;; t-s slot
  (:method ((instance persistent-object) (slot persistent-slot-definition-t))
    (bind ((slot-name (slot-definition-name slot)))
      (cond
        ((and (prc::temporal-p slot) (prc::time-dependent-p slot))
         (sort-entries-by-t
          (collect-if #L(and (eq slot-name (he-slot-name !1))
                             (p-eq instance (he-instance !1))
                             (timestamp<= (he-t-value !1) *t*)
                             (timestamp< (he-validity-start !1) *validity-end*)
                             (timestamp< *validity-start* (he-validity-end !1)))
                      *history-entries*)
          :ascending #t))
        ((prc::temporal-p slot)
         (take 1
               (sort-entries-by-t
                (collect-if #L(and (eq slot-name (he-slot-name !1))
                                   (p-eq instance (he-instance !1))
                                   (timestamp<= (he-t-value !1) *t*))
                            *history-entries*)
                :ascending #f)))
        ((prc::time-dependent-p slot)
         (sort-entries-by-step
          (collect-if #L(and (eq slot-name (he-slot-name !1))
                             (p-eq instance (he-instance !1))
                             (timestamp< (he-validity-start !1) *validity-end*)
                             (timestamp< *validity-start* (he-validity-end !1)))
                      *history-entries*)
          :ascending #t))
        (t
         (error "Bug")))))

  ;; t-s association-end
  (:method ((instance persistent-object) (slot persistent-association-end-slot-definition-t))
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot))))
      (cond
        ((and (prc::temporal-p slot) (prc::time-dependent-p slot))
         (sort-entries-by-t
          (collect-if #L(and (or (eq slot-name (he-slot-name !1))
                                 (eq other-slot-name (he-slot-name !1)))
                             (timestamp<= (he-t-value !1) *t*)
                             (timestamp< (he-validity-start !1) *validity-end*)
                             (timestamp< *validity-start* (he-validity-end !1)))
                      *history-entries*)
          :ascending #t))
        ((prc::temporal-p slot)
         (sort-entries-by-t
          (collect-if #L(and (or (eq slot-name (he-slot-name !1))
                                 (eq other-slot-name (he-slot-name !1)))
                             (timestamp<= (he-t-value !1) *t*))
                      *history-entries*)
          :ascending #t))
        ((prc::time-dependent-p slot)
         (sort-entries-by-step
          (collect-if #L(and (or (eq slot-name (he-slot-name !1))
                                 (eq other-slot-name (he-slot-name !1)))
                             (timestamp< (he-validity-start !1) *validity-end*)
                             (timestamp< *validity-start* (he-validity-end !1)))
                      *history-entries*)
          :ascending #t))
        (t
         (error "Bug"))))))

(defun default-value-for-slot (slot)
  (bind (((:values slot-default-value has-default-p) (prc::default-value-for-type (prc::canonical-type-of slot)))
         (default-value (if has-default-p slot-default-value +unbound-slot-marker+)))
    (if (and (typep slot 'prc::persistent-slot-definition-t)
             (prc::time-dependent-p slot))
        (make-single-values-having-validity default-value *validity-start* *validity-end*)
        default-value)))

(defun execute-history-entries (history-entries instance slot)
  "Executes the given history entries on the slot and returns the slot-value."
  (iter (with slot-value = (default-value-for-slot slot))
        (for entry :in history-entries)
        (setf slot-value (execute-history-entry instance slot slot-value entry))
        (finally (return slot-value))))

(defmacro lift-values-having-validity ((slot-value &rest variables) &body body)
  (bind ((variable-names (mapcar #'first variables))
         (missing-value (cons nil nil))
         (variable-specs (mapcar #L(list !1 :default `',missing-value) variable-names)))
    `(typecase ,slot-value
      (values-having-validity
       (bind (,@(mapcar #L(list (first !1)
                                `(make-single-values-having-validity ,(first !1) ,(second !1) ,(third !1)))
                        variables))
         (iter (for (validity-start validity-end (,slot-value :skip-if-missing #t) ,@variable-specs) :in-values-having-validity (,slot-value ,@variable-names))
               (collect-value-with-validity
                (if (or ,@(mapcar (lambda (name) `(eq ,name ',missing-value)) variable-names))
                    ,slot-value (progn ,@body))
                from validity-start to validity-end))))
      (t
       (progn ,@body)))))

(defgeneric execute-history-entry (instance slot slot-value entry)
  (:documentation "Returns the new slot-value.")

  ;; slot
  (:method ((instance persistent-object) (slot persistent-slot-definition) slot-value entry)
    (if (and (p-eq instance (he-instance entry))
             (eq (slot-definition-name slot) (he-slot-name entry)))

        (bind ((he-value (he-value entry))
               (he-validity-start (he-validity-start entry))
               (he-validity-end (he-validity-end entry)))
          (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
            he-value))
        slot-value))

  ;; association-end
  (:method ((instance persistent-object) (slot persistent-association-end-slot-definition) slot-value entry)
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot)))
           (association-kind (prc::association-kind-of (prc::association-of slot)))
           (slot-cardinality-kind (prc::cardinality-kind-of slot))
           (he-instance (he-instance entry))
           (he-value (he-value entry))
           (he-slot-name (he-slot-name entry))
           (he-action (he-action entry))
           (he-validity-start (he-validity-start entry))
           (he-validity-end (he-validity-end entry)))
      (ecase association-kind
        (:1-1
         ;;  A----EV       EI: he-instance
         ;;      /         EV: he-value
         ;;     /          I:  instance
         ;;    /           V:  slot-value
         ;;  EI----B
         (cond
           ;; set slot of this instance (I=EI)
           ((and (eq slot-name he-slot-name)
                 (p-eq instance he-instance))
            (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
              he-value))
           ;; set other-slot of some other instance to this instance (I=EV)
           ((and (eq other-slot-name he-slot-name)
                 he-value
                 (p-eq instance he-value))
            (lift-values-having-validity (slot-value (he-instance he-validity-start he-validity-end))
              he-instance))
           ;; set slot of some other instance to the same value as this instance had-> clear slot value (I=A)
           ((and (eq slot-name he-slot-name)
                 he-value)
            (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
              (if (p-eq slot-value he-value)
                  nil
                  slot-value)))
           ;; set other slot of the value this instance had -> clear slot value (I=B)
           ((eq other-slot-name he-slot-name)
            (lift-values-having-validity (slot-value (he-instance he-validity-start he-validity-end))
              (if (p-eq slot-value he-instance)
                  nil
                  slot-value)))
           (t
            slot-value)))
        (:1-n
         (ecase slot-cardinality-kind
           ;; instance = parent
           (:n
            ;;  A----EV       EI: he-instance
            ;;      /         EV: he-value
            ;;     /          I:  instance
            ;;    /           V:  slot-value
            ;;  EI----B
            (cond
              ;; set/insert/delete children of this instance (I=EI)
              ((and (eq slot-name he-slot-name)
                    (p-eq instance he-instance))
               (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
                 (ecase he-action
                   (:set he-value)
                   (:insert (adjoin he-value slot-value :test #'p-eq))
                   (:delete (remove he-value slot-value :test #'p-eq)))))
              ;; set parent of some child to this instance (I=EV)
              ((and (eq other-slot-name he-slot-name)
                    he-value
                    (p-eq instance he-value))
               (lift-values-having-validity (slot-value (he-instance he-validity-start he-validity-end))
                 (adjoin he-instance slot-value :test #'p-eq)))
              ;; set/add children of some other instance and this instance had those children -> remove children (I=A)
              ((and (eq slot-name he-slot-name)
                    he-value)
               (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
                 (ecase he-action
                   (:set (set-difference slot-value he-value :test #'p-eq))
                   (:insert (remove he-value slot-value :test #'p-eq))
                   (:delete slot-value))))
              ;; set parent of some current child to another one (or null) -> remove that child (I=B)
              ((eq other-slot-name he-slot-name)
               (lift-values-having-validity (slot-value (he-instance he-validity-start he-validity-end))
                 (remove he-instance slot-value :test #'p-eq)))
              (t
               slot-value)))
           ;; instance = child
           (:1
            ;;  A----EV       EI: he-instance
            ;;      /         EV: he-value
            ;;     /          I:  instance
            ;;    /           V:  slot-value
            ;;  EI----B
            (cond
              ;; set parent of this instance (I=EI)
              ((and (eq slot-name he-slot-name)
                    (p-eq instance he-instance))
               (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
                 he-value))
              ;; set children
              ((and (eq other-slot-name he-slot-name)
                    slot-value)
               (lift-values-having-validity (slot-value (he-instance he-validity-start he-validity-end)
                                                        (he-value he-validity-start he-validity-end))
                 (if (p-eq slot-value he-instance)
                     ;; set/remove children of the current parent -> clear parent (I=B)
                     (ecase he-action
                       (:set (if (member instance he-value :test #'p-eq) slot-value nil))
                       (:insert slot-value)
                       (:delete (if (p-eq instance he-value) nil slot-value)))
                     ;; set/insert this child to some other parent (I=EV)
                     (ecase he-action
                       (:set (if (member instance he-value :test #'p-eq) he-instance slot-value))
                       (:insert (if (p-eq instance he-value) he-instance slot-value))
                       (:delete slot-value)))))
              ;; set/insert this child to some other parent (I=EV)
              ((and (eq other-slot-name he-slot-name)
                    he-value)
               (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
                 (ecase he-action
                   (:set (if (member instance he-value :test #'p-eq) he-instance slot-value))
                   (:insert (if (p-eq instance he-value) he-instance slot-value))
                   (:delete slot-value))))
              (t
               slot-value)))))
        (:m-n
         (cond
           ;; set this slot (I=EI)
           ((and (eq slot-name he-slot-name)
                 (p-eq instance he-instance))

            (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end))
              (ecase he-action
                (:set he-value)
                (:insert (adjoin he-value slot-value :test #'p-eq))
                (:delete (remove he-value slot-value :test #'p-eq)))))
           ;; set other slot (I=EV)
           ((eq other-slot-name he-slot-name)
            (lift-values-having-validity (slot-value (he-value he-validity-start he-validity-end)
                                                     (he-instance he-validity-start he-validity-end))
              
              (ecase he-action
                (:set
                 (cond
                   ((member instance he-value :test #'p-eq)
                    (adjoin he-instance slot-value :test #'p-eq))
                   ((member he-instance slot-value :test #'p-eq)
                    (remove he-instance slot-value :test #'p-eq))
                   (t
                    slot-value)))
                (:insert
                 (cond
                   ((p-eq instance he-value)
                    (adjoin he-instance slot-value :test #'p-eq))
                   (t
                    slot-value)))
                (:delete
                 (cond
                   ((p-eq instance he-value)
                    (remove he-instance slot-value :test #'p-eq))
                   (t
                    slot-value))))))
           (t
            slot-value)))))))

(defun slot-value* (instance slot-name)
  (bind ((slot (find-slot (class-of instance) slot-name))
         (entries (filter-and-sort-history-entries instance slot))
         (value (execute-history-entries entries instance slot)))

    (if (and (values-having-validity-p value)
             (iter (for (s e v) :in-values-having-validity value)
                   (thereis (unbound-slot-marker-p v)))) 
        +unbound-slot-marker+
        value)))

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
    (when (or (typep (find-slot (class-of instance) slot-name) 'persistent-association-end-slot-definition-t)
              (not (find-item (slot-value instance slot-name) item)))
      (insert-item (slot-value instance slot-name) item))
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
    (when (or (typep (find-slot (class-of instance) slot-name) 'persistent-association-end-slot-definition-t)
              (find-item (slot-value instance slot-name) item))
      (delete-item (slot-value instance slot-name) item))
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
                (unless (starts-with-subseq "H-" (symbol-name slot-name)) ;was (eq slot-name 'h-objects)
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
                  (bind ((value (slot-value instance slot-name)))
                    (when (values-having-validity-p value)
                      (assert (single-values-having-validity-p value))
                      (setf value (single-values-having-validity-value value)))
                    value)
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

    ;; TODO slot-value should consolidate
    (when (values-having-validity-p persistent-value)
      (setf persistent-value (consolidate-values-having-validity persistent-value :test #'compare)))
    (when (values-having-validity-p test-value)
      (setf test-value (consolidate-values-having-validity test-value :test #'compare)))

    
    (cond
      ((and (values-having-validity-p persistent-value)
            (values-having-validity-p test-value))
       (iter (for (persistent-validity-start persistent-validity-end persistent-value) :in-values-having-validity persistent-value)
             (for (test-validity-start test-validity-end test-value) :in-values-having-validity test-value)
             (always (and (compare persistent-value test-value)
                          (timestamp= persistent-validity-start test-validity-start)
                          (timestamp= persistent-validity-end test-validity-end)))))
      (t
       (compare persistent-value test-value)))))

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
                           (mapcan #L(list (adjust-timestamp !1 (offset :nsec (- epsilon-nsec)))
                                           !1
                                           (adjust-timestamp !1 (offset :nsec epsilon-nsec)))
                                   timestamps)
                           timestamps)
                       (list +end-of-time+))))
           (fixup-timestamps (timestamps)
             (sort (delete-duplicates (delete-if #L(or (timestamp< !1 +beginning-of-time+)
                                                       (timestamp< +end-of-time+ !1))
                                                 (extend-timestamps timestamps))
                                      :test #'timestamp=)
                   #'timestamp<)))
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
                        (with-transaction
                          (with-t t-value
                            (with-validity-range validity-start validity-end
                              (compare-history instances :slot-names slot-names))))))))))

(defun random-universal-time ()
  (random 5000000000))

(defun random-timestamp ()
  (universal-to-timestamp (random-universal-time)))

(defmacro with-random-t (&body forms)
  `(with-t (random-timestamp)
     ,@forms))

(defmacro with-random-validity-range (&body forms)
  `(bind ((validity-start (random-timestamp))
          (validity-end (adjust-timestamp validity-start (offset :sec (random-universal-time)))))
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
            (generate-instances class-names instance-count :slot-names slot-names))))
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
                                                        (make-instance ',(class-name (class-of instance))))))))
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
                                               (collect (random-timestamp)))
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
                                        (until (timestamp> end start))
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
                 (compare-history instances :slot-names slot-names))
               (iter (repeat random-test-count)
                     ;; default x random
                     (with-transaction
                       (with-random-validity-range
                         (compare-history instances :slot-names slot-names)))
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
