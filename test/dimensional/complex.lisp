;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec-test)

;;;;;;;;;;;
;;; Complex

(def special-variable *history-entry-counter*)

(def special-variable *history-entries*)

(def special-variable *transaction-counter*)

(def special-variable *test-instance* nil)

(def special-variable *test-slot-name* nil)

(defstruct (history-entry (:conc-name he-))
  (step (incf *history-entry-counter*) :type integer)
  (transaction-index *transaction-counter* :type integer)
  (action nil :type (member :set :insert :delete))
  (instance nil :type (or null persistent-object))
  (slot-name nil :type symbol)
  (d-value nil :type prc::d-value))

(def (function io) he-dimensions (entry)
  (prc::dimensions-of (he-d-value entry)))

(def (function io) he-coordinates (entry)
  (prc::coordinates-of (first (prc::c-values-of (he-d-value entry)))))

(def (function io) he-value (entry)
  (prc::value-of (first (prc::c-values-of (he-d-value entry)))))

(def (function i) he-time (entry)
  (bind ((index (position *time-dimension* (he-dimensions entry))))
    (if index
        (coordinate-range-begin (elt (he-coordinates entry) index))
        nil)))

(def (function i) he-validity-begin (entry)
  (bind ((index (position *validity-dimension* (he-dimensions entry))))
    (if index
        (coordinate-range-begin (elt (he-coordinates entry) index))
        nil)))

(def (function i) he-validity-end (entry)
  (bind ((index (position *validity-dimension* (he-dimensions entry))))
    (if index
        (coordinate-range-end (elt (he-coordinates entry) index))
        nil)))

(def (function io) dimensions-of* (slot)
  (if (typep slot 'prc::persistent-slot-definition-d)
      (prc::dimensions-of slot)
      nil))

(def function collect-coordinates (instance slot-name)
  (bind ((slot (find-slot (class-of instance) slot-name))
         (dimensions (dimensions-of* slot)))
    (prc::collect-coordinates-from-variables dimensions)))

(def (function io) call-with-function-of (dimension)
  (symbol-function (format-symbol (symbol-package (name-of dimension)) "CALL-WITH-~A" (name-of dimension))))

(def (function io) call-with-range-function-of (dimension)
  (symbol-function (format-symbol (symbol-package (name-of dimension)) "CALL-WITH-~A-RANGE" (name-of dimension))))

(def function call-with-coordinates (dimensions coordinates thunk)
  (flet ((coerce-coordinate (coordinate type) ; TODO make these coercions in with-coordinates
           (if (coordinate-range-p coordinate)
               (make-coordinate-range
                (coordinate-range-bounds coordinate)
                (prc::coerce-to-coordinate (coordinate-range-begin coordinate) type)
                (prc::coerce-to-coordinate (coordinate-range-end coordinate) type))
               (prc::coerce-to-coordinate coordinate type))))
    (if (null dimensions)
        (progn
          (assert (null coordinates))
          (funcall thunk))
        (bind ((dimension (lookup-dimension (first dimensions)))
               (type (prc::the-type-of dimension))
               (coordinate (coerce-coordinate (first coordinates) type)))
          (typecase dimension
            (ordering-dimension
             (funcall (call-with-range-function-of dimension)
                      (prc::coordinate-range-begin coordinate)
                      (prc::coordinate-range-end coordinate)
                      (lambda ()
                        (call-with-coordinates (rest dimensions)
                                               (rest coordinates)
                                               thunk))))
            (t
             (funcall (call-with-function-of dimension)
                      coordinate
                      (lambda ()
                        (call-with-coordinates (rest dimensions)
                                               (rest coordinates)
                                               thunk)))))))))

(def macro with-coordinates (dimensions coordinates &body forms)
  `(call-with-coordinates
    ',dimensions
    ',coordinates
    (lambda ()
      ,@forms)))

(def function sort-entries-by-step (entries &key (ascending #t))
  (sort entries (if ascending #'< #'>) :key #'he-step))

(def function sort-entries-by-coordinate (entries coordinate-index &key (ascending #t))
  (sort entries
        (if ascending
            (lambda (he-1 he-2)
              (bind ((coordinate-1 (coordinate-range-begin (elt (he-coordinates he-1) coordinate-index)))
                     (coordinate-2 (coordinate-range-begin (elt (he-coordinates he-2) coordinate-index))))
                (or (coordinate< coordinate-1 coordinate-2)
                    (and (coordinate= coordinate-1 coordinate-2)
                         (< (he-step he-1) (he-step he-2))))))
            (lambda (he-1 he-2)
              (bind ((coordinate-1 (coordinate-range-begin (elt (he-coordinates he-1) coordinate-index)))
                     (coordinate-2 (coordinate-range-begin (elt (he-coordinates he-2) coordinate-index))))
                (or (coordinate< coordinate-2 coordinate-1)
                    (and (coordinate= coordinate-2 coordinate-1)
                        (< (he-step he-2) (he-step he-1)))))))))

(def function take (n list)
  (labels ((recurse (n list result)
             (cond
               ((zerop n) result)
               ((null list) result)
               (t (recurse (1- n) (rest list) (cons (first list) result))))))
    (nreverse (recurse n list nil))))

(defgeneric filter-and-sort-history-entries (instance slot coordinates)

  ;; normal slot
  (:method ((instance persistent-object) (slot persistent-slot-definition) coordinates)
    (bind ((slot-name (slot-definition-name slot)))
      (take 1
            (sort-entries-by-step
             (collect-if #L(and (eq slot-name (he-slot-name !1))
                                (p-eq instance (he-instance !1)))
                         *history-entries*)
             :ascending #f))))

  ;; normal association-end
  (:method ((instance persistent-object) (slot persistent-association-end-slot-definition) coordinates)
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot))))
      (sort-entries-by-step
       (collect-if #L(or (eq slot-name (he-slot-name !1))
                         (eq other-slot-name (he-slot-name !1)))
                   *history-entries*)
       :ascending #t)))

  ;; d-s slot
  (:method ((instance persistent-object) (slot persistent-slot-definition-d) coordinates)
    (bind ((slot-name (slot-definition-name slot))
           (dimensions (prc::dimensions-of slot))
           (inheriting-dimension-index (position-if [typep !1 'inheriting-dimension] dimensions))
           (entries (collect-if (lambda (entry)
                                  (and (eq slot-name (he-slot-name entry))
                                       (p-eq instance (he-instance entry))
                                       (coordinates-intersection dimensions coordinates (he-coordinates entry))))
                                *history-entries*)))

      (if inheriting-dimension-index
          (if (and (length= 1 dimensions)
                   (coordinate-range-empty-p (elt coordinates inheriting-dimension-index)))
              (take 1 (sort-entries-by-coordinate entries inheriting-dimension-index :ascending #f))
              (sort-entries-by-coordinate entries inheriting-dimension-index :ascending #t))
          (sort-entries-by-step entries :ascending #t))))

  ;; d-s association-end
  (:method ((instance persistent-object) (slot persistent-association-end-slot-definition-d) coordinates)
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot)))
           (dimensions (prc::dimensions-of slot))
           (inheriting-dimension-index (position-if [typep !1 'inheriting-dimension] dimensions))
           (entries (collect-if (lambda (entry)
                                  (and (or (eq slot-name (he-slot-name entry))
                                           (eq other-slot-name (he-slot-name entry)))
                                       (coordinates-intersection dimensions coordinates (he-coordinates entry))))
                                *history-entries*)))

      (if inheriting-dimension-index
          (sort-entries-by-coordinate entries inheriting-dimension-index :ascending #t)
          (sort-entries-by-step entries :ascending #t)))))

(def function default-value-for-slot (slot coordinates)
  (bind (((slot-default-value . has-default-p) (prc::default-value-for-type-of slot))
         (default-value (if has-default-p slot-default-value +unbound-slot-marker+)))
    (if (and (typep slot 'prc::persistent-slot-definition-d))
        (make-single-d-value (prc::dimensions-of slot) coordinates default-value)
        default-value)))

(def function execute-history-entries (history-entries instance slot coordinates)
  "Executes the given history entries on the slot and returns the slot-value."
  (iter (with slot-value = (default-value-for-slot slot coordinates))
        (for entry :in history-entries)
        (setf slot-value (execute-history-entry entry instance slot slot-value coordinates))
        (finally (return slot-value))))

(def macro lift-values-having-validity ((slot-value &rest variables) &body body)
  "TODO: eliminate the typecase: normal slots values can be 0-dimensional d-values"
  (bind ((variable-names (mapcar #'first variables))
         (missing-value (cons nil nil)))
    `(typecase ,slot-value
       (d-value
        (bind ((dimensions (prc::dimensions-of ,slot-value))
               ,@(mapcar (lambda (variable-spec)
                           `(,(first variable-spec) (make-single-d-value dimensions
                                                                         ,(second variable-spec)
                                                                         ,(first variable-spec))))
                         variables))
          (iter (for (coordinates (,slot-value ,@variable-names)) :in-d-values (,slot-value ,@variable-names) :unspecified-value ',missing-value)
                (when (eq ,slot-value ',missing-value)
                  (next-iteration))
                (collect-d-value
                 (if (or ,@(mapcar (lambda (name) `(eq ,name ',missing-value)) variable-names))
                     ,slot-value (progn ,@body))
                 :dimensions dimensions :coordinates coordinates))))
       (t
        (progn ,@body)))))

(defgeneric execute-history-entry (entry instance slot slot-value coordinates)
  (:documentation "Returns the new slot-value.")

  ;; slot
  (:method (entry (instance persistent-object) (slot persistent-slot-definition) slot-value coordinates)
    (if (and (p-eq instance (he-instance entry))
             (eq (slot-definition-name slot) (he-slot-name entry)))

        (bind ((he-value (he-value entry))
               (he-coordinates (he-coordinates entry)))
          (lift-values-having-validity (slot-value (he-value he-coordinates))
            he-value))
        slot-value))

  ;; association-end
  (:method (entry (instance persistent-object) (slot persistent-association-end-slot-definition) slot-value coordinates)
    (bind ((slot-name (slot-definition-name slot))
           (other-slot-name (slot-definition-name (prc::other-association-end-of slot)))
           (association-kind (prc::association-kind-of (prc::association-of slot)))
           (slot-cardinality-kind (prc::cardinality-kind-of slot))
           (he-instance (he-instance entry))
           (he-value (he-value entry))
           (he-coordinates (he-coordinates entry))
           (he-slot-name (he-slot-name entry))
           (he-action (he-action entry)))
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
            (lift-values-having-validity (slot-value (he-value he-coordinates))
              he-value))
           ;; set other-slot of some other instance to this instance (I=EV)
           ((and (eq other-slot-name he-slot-name)
                 he-value
                 (p-eq instance he-value))
            (lift-values-having-validity (slot-value (he-instance he-coordinates))
              he-instance))
           ;; set slot of some other instance to the same value as this instance had-> clear slot value (I=A)
           ((and (eq slot-name he-slot-name)
                 he-value)
            (lift-values-having-validity (slot-value (he-value he-coordinates))
              (if (p-eq slot-value he-value)
                  nil
                  slot-value)))
           ;; set other slot of the value this instance had -> clear slot value (I=B)
           ((eq other-slot-name he-slot-name)
            (lift-values-having-validity (slot-value (he-instance he-coordinates))
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
               (lift-values-having-validity (slot-value (he-value he-coordinates))
                 (ecase he-action
                   (:set he-value)
                   (:insert (adjoin he-value slot-value :test #'p-eq))
                   (:delete (remove he-value slot-value :test #'p-eq)))))
              ;; set parent of some child to this instance (I=EV)
              ((and (eq other-slot-name he-slot-name)
                    he-value
                    (p-eq instance he-value))
               (lift-values-having-validity (slot-value (he-instance he-coordinates))
                 (adjoin he-instance slot-value :test #'p-eq)))
              ;; set/add children of some other instance and this instance had those children -> remove children (I=A)
              ((and (eq slot-name he-slot-name)
                    he-value)
               (lift-values-having-validity (slot-value (he-value he-coordinates))
                 (ecase he-action
                   (:set (set-difference slot-value he-value :test #'p-eq))
                   (:insert (remove he-value slot-value :test #'p-eq))
                   (:delete slot-value))))
              ;; set parent of some current child to another one (or null) -> remove that child (I=B)
              ((eq other-slot-name he-slot-name)
               (lift-values-having-validity (slot-value (he-instance he-coordinates))
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
               (lift-values-having-validity (slot-value (he-value he-coordinates))
                 he-value))
              ;; set children
              ((and (eq other-slot-name he-slot-name)
                    slot-value)
               (lift-values-having-validity (slot-value (he-instance he-coordinates)
                                                        (he-value he-coordinates))
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
               (lift-values-having-validity (slot-value (he-value he-coordinates))
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

            (lift-values-having-validity (slot-value (he-value he-coordinates))
              (ecase he-action
                (:set he-value)
                (:insert (adjoin he-value slot-value :test #'p-eq))
                (:delete (remove he-value slot-value :test #'p-eq)))))
           ;; set other slot (I=EV)
           ((eq other-slot-name he-slot-name)
            (lift-values-having-validity (slot-value (he-value he-coordinates)
                                                     (he-instance he-coordinates))
              
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

(def function collect-coordinates* (dimensions)
  (iter (for dimension :in dimensions)
        (typecase dimension
          (inheriting-dimension
           (assert (coordinate= (begin-coordinate dimension)
                                (end-coordinate dimension)))
           (collect (make-coordinate-range
                     'ii
                     (begin-coordinate dimension)
                     (prc::maximum-coordinate-of dimension))))
          (ordering-dimension
           (assert (coordinate< (begin-coordinate dimension)
                                (end-coordinate dimension)))
           (collect (make-coordinate-range
                     'ie
                     (begin-coordinate dimension)
                     (end-coordinate dimension))))
          (t
           (collect (coordinate dimension))))))

(def function slot-value* (instance slot-name)
  (bind ((slot (find-slot (class-of instance) slot-name))
         (dimensions (dimensions-of* slot))
         (coordinates (prc::collect-coordinates-from-variables dimensions))
         (entries (filter-and-sort-history-entries instance slot coordinates))
         (value (execute-history-entries entries instance slot coordinates)))

    (if (and (d-value-p value)
             (iter (for (coords v) :in-d-value value)
                   (thereis (unbound-slot-marker-p v)))) 
        +unbound-slot-marker+
        value)))

(def function (setf slot-value*) (new-value instance slot-name)
  (assert (not (d-value-p new-value)))
  (bind ((dimensions (dimensions-of* (find-slot (class-of instance) slot-name)))
         (coordinates (collect-coordinates* dimensions)))
    (push (make-history-entry :action :set
                                     :instance instance
                                     :slot-name slot-name
                                     :d-value (make-single-d-value dimensions coordinates new-value))
         *history-entries*)))

(def function (setf slot-value-and-slot-value*) (new-value instance slot-name)
  (setf (slot-value instance slot-name) new-value)
  (setf (slot-value* instance slot-name) new-value))

(def function insert-item-and-insert-item* (instance slot-name item)
  (with-lazy-slot-value-collections
    (bind ((slot (find-slot (class-of instance) slot-name))
           (dimensions (dimensions-of* slot))
           (coordinates (collect-coordinates* dimensions)))
      (when (or (typep slot 'persistent-association-end-slot-definition-d)
                (not (find-item (slot-value instance slot-name) item)))
        (insert-item (slot-value instance slot-name) item))
      (push (make-history-entry :action :insert
                                :instance instance
                                :slot-name slot-name
                                :d-value (make-single-d-value dimensions coordinates item))
            *history-entries*))))

(def function delete-item-and-delete-item* (instance slot-name item)
  (with-lazy-slot-value-collections
    (bind ((slot (find-slot (class-of instance) slot-name))
           (dimensions (dimensions-of* slot))
           (coordinates (collect-coordinates* dimensions)))
      (when (or (typep slot 'persistent-association-end-slot-definition-d)
                (find-item (slot-value instance slot-name) item))
        (delete-item (slot-value instance slot-name) item))
      (push (make-history-entry :action :delete
                                :instance instance
                                :slot-name slot-name
                                :d-value (make-single-d-value dimensions coordinates item))
            *history-entries*))))

(def function complex-test-slot-names (class slot-names)
  (bind ((available-slot-names
          (iter (for slot :in (prc::persistent-effective-slots-of class))
                (for slot-name = (slot-definition-name slot))
                (unless (starts-with-subseq "H-" (symbol-name slot-name)) ;was (eq slot-name 'h-instances)
                  (collect slot-name)))))
    (if slot-names
        (intersection slot-names available-slot-names)
        available-slot-names)))

(def function generate-instances (class-names count &key (slot-names nil))
  (iter (repeat count)
        (for class-name = (random-elt class-names))
        (for instance = (make-instance* class-name :slot-names slot-names))
        (format t "Generated instance ~A~%" instance)
        (collect instance)))

(def function make-instance* (class-name &key (slot-names nil))
  (iter (with instance = (make-instance class-name))
        (for slot-name :in (complex-test-slot-names (class-of instance) slot-names))
        (setf (slot-value* instance slot-name)
              (if (slot-boundp instance slot-name)
                  (bind ((value (slot-value instance slot-name)))
                    (assert (or (not (d-value-p value)) (single-d-value-p value)))
                    (if (d-value-p value)
                        (single-d-value value)
                        value))
                  +unbound-slot-marker+))
        (finally (return instance))))

(def function compare-persistent-and-test-values (persistent-value test-value)
  (flet ((compare (persistent-value test-value)
           (cond ((and (typep persistent-value 'persistent-object)
                       (typep test-value 'persistent-object))
                  (p-eq persistent-value test-value))
                 ((and (listp persistent-value)
                       (listp test-value))
                  (every #'p-eq
                         (sort (copy-seq persistent-value) #'< :key #'oid-of)
                         (sort (copy-seq test-value) #'< :key #'oid-of)))
                 (t (eql persistent-value test-value)))))

    (cond
      ((and (d-value-p persistent-value)
            (d-value-p test-value))
       (d-value-equal persistent-value test-value :test #'compare))
      (t
       (compare persistent-value test-value)))))

(def function assert-persistent-and-test-values (instance slot-name persistent-value test-value)
  (is (compare-persistent-and-test-values persistent-value test-value)
      "The persistent value: ~A and test value: ~A are different~%in the slot ~A of ~A~%with coordinates ~A~%with ~A history entries: ~A"
      persistent-value test-value slot-name instance (collect-coordinates instance slot-name)
      (length *history-entries*) *history-entries*))

(def function compare-history (instances &key (slot-names nil))
  (iter (for instance :in instances)
        (for class = (class-of instance))
        (revive-instance instance)
        (iter (for slot-name :in (complex-test-slot-names (class-of instance) slot-names))
              (bind ((*test-instance* instance)
                     (*test-slot-name* slot-name)
                     (persistent-value (if (slot-boundp instance slot-name)
                                          (slot-value instance slot-name)
                                          +unbound-slot-marker+))
                     (test-value (slot-value* instance slot-name)))
                (assert-persistent-and-test-values instance slot-name persistent-value test-value)))))

(def function full-compare-history (instances &key (slot-names nil) (add-epsilon-timestamps t))
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
                                                 (extend-timestamps (remove nil timestamps)))
                                      :test #'timestamp=)
                   #'timestamp<)))
    (bind ((time-values (fixup-timestamps
                         (mapcar 'he-time *history-entries*)))
           (validity-values (fixup-timestamps
                             (append (mapcar 'he-validity-begin *history-entries*)
                                     (mapcar 'he-validity-end *history-entries*)))))
      (format t "~&Time values: ~A" time-values)
      (format t "~&Validity values: ~A~%" validity-values)
      (iter (with count = 0)
            (with total = (* (length time-values)
                             (/ (* (length validity-values)
                                   (1- (length validity-values)))
                                2)))
            (for time-value :in time-values)
            (iter (for validity-begin-list :on validity-values)
                  (for validity-begin = (car validity-begin-list))
                  (iter (for validity-end :in (cdr validity-begin-list))
                        (when (zerop (mod count 100))
                          (format t "~&At: ~d/~d" count total))
                        (incf count)
                        (with-transaction
                          (with-time time-value
                            (with-validity-range validity-begin validity-end
                              (compare-history instances :slot-names slot-names))))))))))

(def function random-universal-time ()
  (1+ (random 5000000000)))

(def function random-timestamp (&optional choices)
  (if choices
      (random-elt choices)
      (universal-to-timestamp (random-universal-time))))

(def function random-timestamp-interval (&optional choices (empty-interval-probability 0.0))
  (cond
    ((< (random 1.0) empty-interval-probability)
     (bind ((begin (random-timestamp choices)))
       (cons begin begin)))
    ((null choices)
     (bind ((begin (random-timestamp))
            (offset (random-universal-time)))
       (cons begin (adjust-timestamp begin (offset :sec offset)))))
    (t
     (assert (not (length= 1 choices)))
     (iter (for begin = (random-timestamp choices))
           (for end = (random-timestamp choices))
           (when (timestamp< begin end)
             (leave (cons begin end)))))))

(def function random-coordinate (dimension &optional for-writing-p choices
                                           (empty-interval-probability 0.0))
  (bind ((type (prc::the-type-of dimension)))
   (flet ((random-coordinate ()
            (case type
              (timestamp (make-empty-coordinate-range (random-timestamp choices)))
              (t (error "TODO"))))
          (random-coordinate-range ()
            (case type
              (timestamp
               (bind (((begin . end) (random-timestamp-interval choices empty-interval-probability)))
                 (if (timestamp= begin end)
                     (make-empty-coordinate-range begin)
                     (make-coordinate-range 'ie begin end))))
              (t (error "TODO")))))
     (etypecase dimension
       (inheriting-dimension
        (if for-writing-p
            (random-coordinate)
            (random-coordinate-range)))
       (ordering-dimension
        (if for-writing-p
            (random-coordinate)
            (random-coordinate-range)))
       (dimension
        (random-coordinate))))))

(def function call-with-random-coordinates (dimensions thunk)
  (bind ((coordinates (mapcar #'random-coordinate dimensions)))
    (call-with-coordinates dimensions coordinates thunk)))

(def macro with-random-coordinates (dimensions &body forms)
  `(call-with-random-coordinates
    ,dimensions
    (lambda ()
      ,@forms)))

(def function do-random-operation (instances &key (slot-names nil))
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
    (format t "+ ~A slot ~A of ~A~%  with coordinates ~A the value ~A~%"
            (ecase action
              (:set "Setting")
              (:insert "Inserting into")
              (:delete "Deleting from"))
            slot-name instance (collect-coordinates instance slot-name) slot-value)
    (ecase action
      (:set (setf (slot-value-and-slot-value* instance slot-name) slot-value))
      (:insert (insert-item-and-insert-item* instance slot-name slot-value))
      (:delete (delete-item-and-delete-item* instance slot-name slot-value)))))

(def function run-complex-test (&key (class-name nil) (class-names (when class-name (list class-name))) (instance-count 1) (operation-count 1) (transaction-count 1) (timestamp-count 10)
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
            (labels ((instance-variable-name (instance)
                       (concatenate-symbol "instance-" (oid-of instance)))
                     (format-value (value)
                       (cond ((typep value 'persistent-object)
                              (instance-variable-name value))
                             ((listp value)
                              `(list ,@(mapcar #'instance-variable-name value)))
                             (t value)))
                     (format-coordinate (coordinate)
                       (typecase coordinate
                         (cons (cons
                                (coordinate-range-bounds coordinate)
                                (cons (format-coordinate (coordinate-range-begin coordinate))
                                      (format-coordinate (coordinate-range-end coordinate)))))
                         (timestamp (format-timestring nil coordinate :timezone +utc-zone+))
                         (t coordinate))) ; TODO
                     (format-coordinate-range (begin end)
                       (cons
                        (if (coordinate= begin end) 'ii 'ie)
                        (cons (format-coordinate begin)
                              (format-coordinate end))))
                     (format-coordinates (coordinates)
                       (mapcar #'format-coordinate coordinates))
                     (format-dimensions (dimensions)
                       (mapcar #'name-of dimensions))
                     (transform-coordinates (dimensions coordinates)
                       (iter (for dimension :in dimensions)
                             (for coordinate :in coordinates)
                             (collect
                                 (typecase dimension
                                   (inheriting-dimension
                                    (make-coordinate-range
                                     (coordinate-range-bounds coordinate)
                                     (coordinate-range-begin coordinate)
                                     (coordinate-range-begin coordinate)))
                                   (t coordinate))))))
              (bind ((*print-level* nil)
                     (*print-length* nil)
                     (*print-lines* nil)
                     (*print-circle* #f)
                     (slot-name *test-slot-name*)
                     (instance-variable-name (when *test-instance* (instance-variable-name *test-instance*))))
                (format t "~%~S"
                        `(def test test/dimensional/complex/generated ()
                           ,(format nil "~A" error)
                           (bind ((*history-entries* nil)
                                  (*history-entry-counter* 0)
                                  (*transaction-counter* 0)
                                  ,@(iter (for instance :in instances)
                                          (collect `(,(instance-variable-name instance)
                                                      (with-transaction
                                                        (make-instance* ',(class-name (class-of instance))))))))
                             ,@(iter (for transaction-counter :from 0 :to *transaction-counter*)
                                     (for history-entries = (collect-if #L(= transaction-counter (he-transaction-index !1)) *history-entries*))
                                     (when history-entries
                                       (appending
                                        `((with-transaction
                                            (with-revived-instances ,(mapcar #'instance-variable-name instances)
                                              ,@(iter (for entry :in history-entries)
                                                      (for value = (format-value (he-value entry)))
                                                      (for instance = (instance-variable-name (he-instance entry)))
                                                      (for slot-name = (he-slot-name entry))
                                                      (for dimensions = (he-dimensions entry))
                                                      (for coordinates = (transform-coordinates
                                                                          dimensions
                                                                          (he-coordinates entry)))
                                                      (collect
                                                          `(with-coordinates
                                                               ,(format-dimensions dimensions)
                                                               ,(format-coordinates coordinates)
                                                             ,(ecase (he-action entry)
                                                                     (:set `(setf (slot-value-and-slot-value* ,instance ',slot-name) ,value))
                                                                     (:insert `(insert-item-and-insert-item* ,instance ',slot-name ,value))
                                                                     (:delete `(delete-item-and-delete-item* ,instance ',slot-name ,value))))))))
                                          (incf *transaction-counter*)))))
                             (setf *history-entries* (nreverse *history-entries*))
                             ,@(when (and instance-variable-name slot-name)
                                     `((with-transaction
                                         (with-revived-instance ,instance-variable-name
                                           (with-coordinates
                                               (time validity)
                                               (,(format-coordinate-range (begin-coordinate 'time)
                                                                          (end-coordinate 'time))
                                                 ,(format-coordinate-range (begin-coordinate 'validity)
                                                                           (end-coordinate 'validity)))
                                             (bind ((persistent-value
                                                     (if (slot-boundp ,instance-variable-name ',slot-name)
                                                         (slot-value ,instance-variable-name ',slot-name)
                                                         +unbound-slot-marker+))
                                                    (test-value
                                                     (slot-value* ,instance-variable-name ',slot-name)))
                                               (assert-persistent-and-test-values
                                                ,instance-variable-name ',slot-name
                                                persistent-value test-value)))))))))))))
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
              (bind (((time-begin . time-end) (random-timestamp-interval timestamps 1.0))
                     ((validity-begin . validity-end) (random-timestamp-interval timestamps 0.0)))
                (with-transaction
                  (format t "Starting new transaction~%")
                  (incf *transaction-counter*)
                  (iter (repeat operation-count)
                        (with-time-range time-begin time-end
                          (with-validity-range validity-begin validity-end
                            (do-random-operation instances :slot-names slot-names))))))
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
                       (with-random-coordinates (list *validity-dimension*)
                         (compare-history instances :slot-names slot-names)))
                     ;; random x default
                     (with-transaction
                       (with-random-coordinates (list *time-dimension*)
                         (compare-history instances :slot-names slot-names)))
                     ;; random x random
                     (with-transaction
                       (with-random-coordinates (list *time-dimension* *validity-dimension*)
                         (compare-history instances :slot-names slot-names))))))))))

(def test run-complex-tests (&rest args &key (instance-count 10) (operation-count 10) &allow-other-keys)
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
