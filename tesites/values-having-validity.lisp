;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-perec)

;;;;;;;;;
;;; Model

;;; TODO change the representation: one adjustable vector of structs containing the three fields

(defclass* values-having-validity ()
  ((values :type (vector t))
   (validity-starts :type (vector timestamp))
   (validity-ends :type (vector timestamp))))

(def (function e) values-having-validity-p (instance)
  (typep instance 'values-having-validity))

(def (function e) elt-values-having-validity (instance index)
  (values (elt (values-of instance) index)
          (elt (validity-starts-of instance) index)
          (elt (validity-ends-of instance) index)))

(def (function e) values-having-validity= (values-1 values-2 &key (test #'equal))
  (and (length (values-of values-1))
       (length (values-of values-2))
       (iter (for index :from 0 :below (length (values-of values-1)))
             (bind (((:values v-1 s-1 e-1)
                     (elt-values-having-validity values-1 index))
                    ((:values v-2 s-2 e-2)
                     (elt-values-having-validity values-2 index)))
               (always (funcall test v-1 v-2))
               (always (timestamp= s-1 s-2))
               (always (timestamp= e-1 e-2))))))

(def (function e) make-empty-values-having-validity ()
  (make-instance 'values-having-validity
                 :values #()
                 :validity-starts #()
                 :validity-ends #()))

(def (function e) make-single-values-having-validity (value validity-start validity-end)
  (flet ((make-single-element-vector (element)
           (aprog1 (make-array 1)
             (setf (aref it 0) element))))
    (make-instance 'values-having-validity
                   :values (make-single-element-vector value)
                   :validity-starts (make-single-element-vector validity-start)
                   :validity-ends (make-single-element-vector validity-end))))

(def (function e) ensure-values-having-validity (value validity-start validity-end)
  (if (values-having-validity-p value)
      value
      (make-single-values-having-validity value validity-start validity-end)))

(def (function e) single-values-having-validity-p (instance)
  (and (values-having-validity-p instance)
       (= 1 (length (values-of instance)))))

(def (function e) single-values-having-validity-value (instance)
  (assert (single-values-having-validity-p instance))
  (elt-values-having-validity instance 0))

(def (function e) make-values-having-validity (values validity-starts validity-ends)
  (make-instance 'values-having-validity
                 :values (coerce values 'simple-vector)
                 :validity-starts (coerce validity-starts 'simple-vector)
                 :validity-ends (coerce validity-ends 'simple-vector)))

(def print-object values-having-validity
  (write-char #\{)
  (iter (for value :in-sequence (values-of -self-))
        (for start :in-sequence (validity-starts-of -self-))
        (for end :in-sequence (validity-ends-of -self-))
        (unless (first-iteration-p)
          (write-string ", "))
        (write value)
        (write-char #\Space)
        (write start)
        (write-char #\Space)
        (write end))
  (write-char #\}))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteration support

(def (constant :test 'equal) +missing-value+ (cons t t))


(defmacro-clause (for variables :in-values-having-validity value-having-validities)
  "Use it like (for (validity-start validity-end value1 ...) :in-values-having-validity v)"
  (assert (and (listp variables) (>= (length variables) 3)))
  
  (bind ((validity-start-variable (first variables))
         (validity-end-variable (second variables))
         (value-variables (cddr variables)))

    (assert (and validity-start-variable validity-end-variable)
            () "Expects (FOR (validity-start validity-end ...) IN-VALUES-HAVING-VALIDITY ...)")
    
    (assert (or (length= 1 value-variables)
                (and (listp value-having-validities)
                     (= (length value-variables) (length value-having-validities)))) ()
                     "Expects the same number of variables as values-having-validities in (FOR (validity-start validity-end variable ...) IN-VALUES-HAVING-VALIDITY values-having-validity ...)")
    
    (if (length= 1 value-variables)
        ;; efficient version for iterating on one values-having-validity
        (with-unique-names (value-having-validity-variable values validity-starts validity-ends index)
          `(progn
             (with ,value-having-validity-variable = ,value-having-validities)
             (with ,values = (values-of ,value-having-validity-variable))
             (with ,validity-starts = (validity-starts-of ,value-having-validity-variable))
             (with ,validity-ends = (validity-ends-of ,value-having-validity-variable))
             (for ,index :index-of-vector ,values)
             (for ,(first value-variables) = (aref ,values ,index))
             (for ,validity-start-variable = (aref ,validity-starts ,index))
             (for ,validity-end-variable = (aref ,validity-ends ,index))))
        ;; iterating on several values-having-validity parallel
        (bind ((actions-if-missing (mapcar (lambda (var)
                                             ;; three option for holes:
                                             ;; error (default option)/ skip / use given default value
                                             (acond
                                               ((not (listp var)) '(:error))
                                               ((member :default var) (list :default (second it)))
                                               ((member :skip-if-missing var) '(:skip))))
                                           value-variables))
               (value-variables (mapcar #L(if (consp !1) (first !1) !1) value-variables)))
          (with-unique-names (generator values-variable)
            `(progn
               (with ,generator = (values-having-validity-generator ,@value-having-validities))
               (for (values ,validity-start-variable ,validity-end-variable ,values-variable) = (funcall ,generator))
               (while ,validity-start-variable)
               ,@(iter (for variable :in value-variables)
                       (for i from 0)
                       (for action :in actions-if-missing)
                       (appending `((for ,variable = (aref ,values-variable ,i))
                                    (when (eq ,variable +missing-value+)
                                      ,(ecase (first action)
                                              (:error `(error "Missing value for ~S in interval [~S,~S)"
                                                              ',variable ,validity-start-variable ,validity-end-variable))
                                              (:skip `(next-iteration))
                                              (:default `(setf ,variable ,(second action))))))))
             
               ;; (format t "Start: ~S End: ~S~%" ,validity-start-variable ,validity-end-variable)
               ))))))


#+nil
(def function merge-validities (&rest values-having-validities)
  (iter (with result = (make-array 0 :adjustable t :fill-pointer 0))
        (for values-having-validity :in values-having-validities)
        (flet ((add-validities (validities)
                 (iter (for validity :in-vector validities)
                       (unless (find validity result :test #'timestamp=)
                         (vector-push-extend validity result)))))
          (add-validities (validity-starts-of values-having-validity))
          (add-validities (validity-ends-of values-having-validity)))
        (finally
         (sort result #'timestamp<)
         (return result))))

(def function values-having-validity-generator (&rest values-having-validities)
  (bind ((length (length values-having-validities))
         (indeces (make-array length :initial-element 0))
         (values (make-array length))
         (validity-start nil)
         (validity-end +end-of-time+))

    (iter (for values-having-validity :in values-having-validities)
          (for validity-starts = (validity-starts-of values-having-validity))
          (unless (emptyp validity-starts)
            (setf validity-end
                  (timestamp-minimum validity-end (first-elt validity-starts)))))

    (labels ((next-index (values-having-validity i)
               (bind ((index (aref indeces i)))
                 (if (and (< index (length (validity-ends-of values-having-validity)))
                          (timestamp<= (aref (validity-ends-of values-having-validity) index)
                                       validity-end))
                     (incf (aref indeces i))
                     index)))
             
             (next ()
               (setf validity-start validity-end)

               ;; find next validity-end, update indeces
               (bind ((next-validity-end +end-of-time+)
                      (has-next-p #f))
                 (iter (for values-having-validity :in values-having-validities)
                       (for i from 0)
                       (for index = (next-index values-having-validity i))
                       (when (>= index (length (validity-starts-of values-having-validity)))
                         (next-iteration))
                       (for start = (aref (validity-starts-of values-having-validity) index))
                       (for end = (aref (validity-ends-of values-having-validity) index))
                       (setf has-next-p #t)
                       (cond
                         ((timestamp< validity-end start)
                          (setf next-validity-end (timestamp-minimum next-validity-end start)))
                         ((timestamp< validity-end end)
                          (setf next-validity-end (timestamp-minimum next-validity-end end)))
                         (t
                          (error "Bug"))))

                 (if has-next-p
                     (setf validity-end next-validity-end)
                     (return-from next nil)))

               (assert (timestamp< validity-start validity-end))
             
               ;; update values
               (iter (for values-having-validity :in values-having-validities)
                     (for i from 0)
                     (for validity-starts = (validity-starts-of values-having-validity))
                     (for index = (aref indeces i))
                     (setf (aref values i)
                           (if (and (< index (length validity-starts))
                                    (timestamp> validity-end (aref validity-starts index)))
                               (aref (values-of values-having-validity) index)
                               +missing-value+)))

               (values validity-start validity-end values)))
      
      #'next)))


(defun normalize-validity (validity)
  (typecase validity
    (timestamp validity)
    (string (parse-timestring validity))
    (t (error "timestamp or string expected in NORMALIZE-VALIDITY but received ~S." validity))))

(export 'collect-value-with-validity)

;;; (COLLECT &optional INTO)
(iter::defclause (collect-value-with-validity expr
                                              &optional
                                              from validity-start
                                              to validity-end
                                              into variable)
  "Collect into a values-having-validity."
  (assert (or (and validity-start validity-end)
              (and (null validity-start) (null validity-end) (= (length expr) 3)))
          () "Wrong COLLECT-VALUE-WITH-VALIDITY clause in iterate. Use 'COLLECT-VALUES-WITH-VALIDITY <value> FROM <validity-start> TO <validity-end>' or 'COLLECT-VALUES-WITH-VALIDITY (<value> <validity-start> <validity-end>)'.")
  
  (let* ((collect-var-spec (or variable iter::*result-var*))
         (collect-var (iter::extract-var collect-var-spec))
         (validity-start-expr (or validity-start (second expr)))
         (validity-end-expr (or validity-end (third expr)))
         (value-expr (if (and validity-start validity-end) expr (first expr))))

    (iter::make-accum-var-binding collect-var-spec nil :collect-value-with-validity)
      
    (when (stringp validity-start-expr)
      (setf validity-start-expr (parse-timestring validity-start-expr)))

    (when (stringp validity-end-expr)
      (setf validity-end-expr (parse-timestring validity-end-expr)))
    
    (iter::return-code
     :initial `((setf ,collect-var (make-instance 'values-having-validity
                                                  :values (make-array 4 :adjustable #t :fill-pointer 0)
                                                  :validity-starts (make-array 4 :adjustable #t :fill-pointer 0)
                                                  :validity-ends (make-array 4 :adjustable #t :fill-pointer 0))))
     :body `((progn
               (vector-push-extend ,value-expr (values-of ,collect-var))
               (vector-push-extend ,(if (typep validity-start-expr 'timestamp)
                                        validity-start-expr
                                        `(normalize-validity ,validity-start-expr))
                                   (validity-starts-of ,collect-var))
               (vector-push-extend ,(if (typep validity-end-expr 'timestamp)
                                        validity-end-expr
                                        `(normalize-validity ,validity-end-expr))
                                   (validity-ends-of ,collect-var)))))))

;;;;;;;;;;;;;;;;;
;;; Getter/setter

(def function values-having-validity-value (values-having-validity requested-validity-start requested-validity-end)
  "Extracts the requested range if possible, secondary value indicates success."
  (bind ((validity-starts (validity-starts-of values-having-validity))
         (validity-ends (validity-ends-of values-having-validity))
         (first-validity-start (aref validity-starts 0))
         (last-validity-end (aref validity-ends (1- (length validity-ends)))))
    (if (and (timestamp<= first-validity-start requested-validity-start)
             (timestamp<= requested-validity-end last-validity-end))
        (bind ((start-index (upper-bound requested-validity-start validity-ends :sort-fn #'timestamp<))
               (end-index (lower-bound requested-validity-end validity-starts :sort-fn #'timestamp<))
               (values (values-of values-having-validity)))
          (values
           (iter (for i from start-index below end-index)
                 (for value = (aref values i))
                 (for validity-start = (timestamp-maximum (aref validity-starts i) requested-validity-start))
                 (for validity-end = (timestamp-minimum (aref validity-ends i) requested-validity-end))
                 ;;(format t "~S ~S~%" validity-start validity-end)
                 (collect-value-with-validity (value validity-start validity-end)))
           #t))
        
        (values (make-empty-values-having-validity) #f))))

(def (function e) (setf values-having-validity-value) (new-value values-having-validity validity-start validity-end)
  (bind ((values (make-array 4 :adjustable #t :fill-pointer 0))
         (validity-starts(make-array 4 :adjustable #t :fill-pointer 0))
         (validity-ends (make-array 4 :adjustable #t :fill-pointer 0))
         (addedp #f))
    (flet ((add (value start end)
             (vector-push-extend value values)
             (vector-push-extend start validity-starts)
             (vector-push-extend end validity-ends)))
      (iter (for (start end value) :in-values-having-validity values-having-validity)

            (cond
              ;; |---|        old
              ;;       |---|  new
              ((timestamp<= end validity-start)
               (add value start end))
              ;;       |---|  old
              ;; |---|        new
              ((timestamp<= validity-end start)
               (unless addedp
                 (add new-value validity-start validity-end)
                 (setf addedp #t))
               (add value start end))
              ;; |----|    old
              ;;   |----|  new
              ((and (timestamp< start validity-start)
                    (timestamp<= end validity-end))
               (add value start validity-start)
               (add new-value validity-start validity-end)
               (setf addedp #t))
              ;; |--------| old
              ;;   |----|   new
              ((timestamp< start validity-start)
               (add value start validity-start)
               (add new-value validity-start validity-end)
               (add value validity-end end)
               (setf addedp #t))
              ;;   |-----| old
              ;; |----|    new
              ((and (timestamp<= validity-start start)
                    (timestamp< validity-end end))
               (unless addedp
                 (add new-value validity-start validity-end)
                 (setf addedp #t))
               (add value validity-end end))
              ;;    |---|    old
              ;; |---------| new
              (t
               (unless addedp
                 (add new-value validity-start validity-end)
                 (setf addedp #t))))
        
            (finally
             (unless addedp
               (add new-value validity-start validity-end))
             (setf (values-of values-having-validity) values
                   (validity-starts-of values-having-validity) validity-starts
                   (validity-ends-of values-having-validity) validity-ends))))))


;;;;;;;;;;;;;;;;;;;;;;;
;;; For primitive types

(def function collect-values-having-validity (value-holders value-function validity-start-function validity-end-function no-value-function requested-validity-start requested-validity-end)
  "From a list of ordered (by t descending) tuples each containing a value, a validity start and a validity end returns the corresponding values-having-validity for the requested range."
  (assert (timestamp< requested-validity-start requested-validity-end))
  (if (zerop (length value-holders))
      (make-single-values-having-validity (funcall no-value-function requested-validity-start requested-validity-end) requested-validity-start requested-validity-end)
      (bind ((values (make-array 4 :adjustable #t :fill-pointer 0))
             (validity-starts (make-array 4 :adjustable #t :fill-pointer 0))
             (validity-ends (make-array 4 :adjustable #t :fill-pointer 0))
             (indices (make-array 4 :adjustable #t :fill-pointer 0)))
        (labels ((push-value-having-validity (value validity-start validity-end)
                   ;;(format t "~%Push Start: ~A End: ~A Value: ~A" value validity-start validity-end)
                   (vector-push-extend value values)
                   (vector-push-extend validity-start validity-starts)
                   (vector-push-extend validity-end validity-ends)
                   (vector-push-extend (length indices) indices))
                 (%collect-values-having-validity (index validity-start validity-end)
                   ;;(format t "~%Collect Index: ~A Start: ~A End: ~A" index  validity-start validity-end)
                   (when (timestamp< validity-start validity-end)
                     (when (< index (length value-holders))
                       (iter (for i :from index :below (length value-holders))
                             (for record = (elt value-holders i))
                             (for record-validity-start = (funcall validity-start-function record))
                             (for record-validity-end = (funcall validity-end-function record))
                             (for merged-validity-start = (timestamp-maximum validity-start record-validity-start))
                             (for merged-validity-end = (timestamp-minimum validity-end record-validity-end))
                             (when (timestamp< merged-validity-start merged-validity-end)
                               (push-value-having-validity (funcall value-function record) merged-validity-start merged-validity-end)
                               (%collect-values-having-validity (1+ i) validity-start merged-validity-start)
                               (%collect-values-having-validity (1+ i) merged-validity-end validity-end)
                               (return-from %collect-values-having-validity))))
                     (push-value-having-validity
                      (funcall no-value-function validity-start validity-end)
                      validity-start validity-end))))
          (%collect-values-having-validity 0 requested-validity-start requested-validity-end)
          (sort indices #'timestamp< :key (lambda (index) (aref validity-starts index)))
          (permute values indices)
          (permute validity-starts indices)
          (permute validity-ends indices)
          (make-instance 'values-having-validity
                         :values values
                         :validity-starts validity-starts
                         :validity-ends validity-ends)))))

;;;;;;;;;;;;;;;;;
;;; For set types

(def constant +t-clear+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-delete+ 1
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 2
  "Constant used to mark RDBMS records for association slots.")

(def function collect-children-having-validity (value-holders value-function validity-start-function validity-end-function action-function requested-validity-start requested-validity-end)
  "Collect children sets as values-having-validity, value-holders must be ordered by t descending."
  (labels ((%collect-children-having-validity (value-holders validity-start validity-end)
             (bind ((set nil))
               (iter (for value :in-sequence value-holders :from (1- (length value-holders)) :downto 0)
                     (when (and (timestamp<= validity-end (funcall validity-end-function value))
                                (timestamp<= (funcall validity-start-function value) validity-start))
                       (ecase (funcall action-function value)
                         (#.+t-clear+ (setf set nil))
                         (#.+t-insert+ (pushnew (funcall value-function value) set))
                         (#.+t-delete+ (deletef set (funcall value-function value))))))
               set)))
    (bind ((validities nil))
      (flet ((push-validity (validity)
               (when (timestamp<= requested-validity-start validity requested-validity-end)
                 (pushnew validity validities :test #'timestamp=))))
        (push-validity requested-validity-start)
        (push-validity requested-validity-end)
        (iter (for value :in-sequence value-holders)
              (push-validity (funcall validity-start-function value))
              (push-validity (funcall validity-end-function value))))
      (setf validities (sort validities #'timestamp<))
      (if (= 2 (length validities))
          (make-single-values-having-validity (%collect-children-having-validity value-holders (first validities) (second validities)) (first validities) (second validities))
          (iter (with value = nil)
                (with previous-value = :unbound)
                (with previous-validity = nil)
                (for validity :in validities)
                (if previous-validity
                    (progn
                      (setf value (%collect-children-having-validity value-holders previous-validity validity))
                      (progn ;;TODO unless (equal value previous-value)
                          (collect-value-with-validity value :from previous-validity :to validity)
                          (setf previous-value value)
                          (setf previous-validity validity)))
                    (setf previous-validity validity)))))))

;;;;;;;;;;;
;;; Utility

;; TODO: review this
;; TODO: this failes when multiple records are present with the same t but overlapping validity ranges
;; (ordering for t does not affect the order of records) THIS MUST BE FORBIDDEN
(def function collect-single-slot-values-having-validity-from-records (instance slot records h-slot value-index)
  (collect-single-slot-values-having-validity
   instance slot records
   (lambda (record)
     (restore-slot-value instance h-slot record value-index))
   (lambda (record)
     (elt record 0))
   (lambda (record)
     (elt record 1))))

(def function collect-single-slot-values-having-validity-from-instances (instance slot h-instances h-slot)
  (collect-single-slot-values-having-validity
   instance slot h-instances
   (lambda (h-instance)
     (underlying-slot-boundp-or-value-using-class (class-of h-instance) h-instance h-slot))
   (lambda (h-instance)
     (validity-start-of h-instance))
   (lambda (h-instance)
     (validity-end-of h-instance))))

(def function collect-multiple-slot-values-having-validity-from-records (instance slots records h-slots)
  (iter (for slot :in slots)
        (for h-slot :in h-slots)
        (for value-index :initially 2 :then (+ value-index (length (columns-of h-slot))))
        (collect (collect-single-slot-values-having-validity-from-records instance slot records h-slot value-index))))

(def function collect-multiple-slot-values-having-validity-from-instances (instance slots h-instances h-slots)
  (iter (for slot :in slots)
        (for h-slot :in h-slots)
        (collect (collect-single-slot-values-having-validity-from-instances instance slot h-instances h-slot))))

(def function collect-single-slot-values-having-validity (instance slot value-holders value-function validity-start-function validity-end-function)
  (collect-values-having-validity
   value-holders value-function validity-start-function validity-end-function
   (lambda (validity-start validity-end)
     (slot-unbound-t instance slot :validity-start validity-start :validity-end validity-end))
   *validity-start* *validity-end*))

;;
;; Utils
;;
(def (function e) map-values-having-validity (function values-having-validity)
  (bind ((length (length (values-of values-having-validity)))
         (values (make-array length))
         (validity-starts (make-array length))
         (validity-ends (make-array length)))
    (iter (for (start end value) :in-values-having-validity values-having-validity)
          (for index from 0)
          (setf (svref values index) (funcall function value)
                (svref validity-starts index) start
                (svref validity-ends index) end))
    (make-values-having-validity values validity-starts validity-ends)))

(def (function e) consolidate-values-having-validity (values-having-validity &key (test #'eql))
  (if (iter (for (start end value) :in-values-having-validity values-having-validity)
            (for prev-value :previous value)
            (for prev-end :previous end)
            (always (or (first-iteration-p)
                        (timestamp/= start prev-end)
                        (not (funcall test value prev-value)))))
      values-having-validity ; already consolidated
      (iter (with values = (make-array 4 :adjustable #t :fill-pointer 0))
            (with validity-starts = (make-array 4 :adjustable #t :fill-pointer 0))
            (with validity-ends = (make-array 4 :adjustable #t :fill-pointer 0))
            (for (start end value) :in-values-having-validity values-having-validity)
            (for prev-value :previous value)
            (for prev-end :previous end)
            (unless (and (not (first-iteration-p))
                         (timestamp= start prev-end)
                         (funcall test value prev-value))
              (progn
                (when prev-end (vector-push-extend prev-end validity-ends))
                (vector-push-extend value values)
                (vector-push-extend start validity-starts)))
            (finally
             (when end (vector-push-extend end validity-ends))
             (return (make-values-having-validity values validity-starts validity-ends))))))

(def (function e) vhv+ (vhv-1 vhv-2)
  (iter (for (start end (v1 :default 0) (v2 :default 0)) :in-values-having-validity (vhv-1 vhv-2))
        (collect-value-with-validity (+ v1 v2) :from start :to end)))

(def (macro e) vhv-incf (place delta)
  `(setf ,place (vhv+ ,place ,delta)))

(def (function e) vhv-sum (&rest vhvs)
  (reduce #'vhv+ vhvs))