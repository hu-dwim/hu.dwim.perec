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
               (always (local-time= s-1 s-2))
               (always (local-time= e-1 e-2))))))

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
        (unless (first-iteration-p)
          (write-string ", "))
        (write value))
  (write-char #\}))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Iteratation support
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
        ;; iterating on several values-having-validity parallel, TODO there is place for optimization here
        (bind ((actions-if-missing (mapcar (lambda (var)
                                             ;; three option for holes:
                                             ;; error (default option)/ skip / use given default value
                                             (acond
                                               ((not (listp var)) :error)
                                               ((member :default var) (list :default (second it)))
                                               ((member :skip-if-missing var) :skip)))
                                           value-variables))
               (value-variables (mapcar #L(if (consp !1) (first !1) !1) value-variables))
               (value-having-validity-variables (mapcar #L(gensym (symbol-name !1)) value-variables)))
          `(progn
             ,@(iter (for variable :in value-having-validity-variables)
                     (for value-having-validity :in value-having-validities)
                     (collect `(with ,variable = ,value-having-validity)))
             (for ,validity-end-variable :in-vector (merge-validities ,@value-having-validity-variables))
             (for ,validity-start-variable :previous ,validity-end-variable)
             (unless ,validity-start-variable (next-iteration))
             ;; (format t "Start: ~S End: ~S~%" ,validity-start-variable ,validity-end-variable)
             ,@(iter (for values-having-validity-variable :in value-having-validity-variables)
                     (for value-with-validity-variable = (gensym (symbol-name values-having-validity-variable)))
                     (for variable :in value-variables)
                     (for action :in actions-if-missing)
                     (collect `(for ,value-with-validity-variable =
                                    ;; be aware: iterates with _one_ value
                                    (values-having-validity-value ,values-having-validity-variable ,validity-start-variable ,validity-end-variable)))
                     
                     (if (member action (list :error :skip))
                         (appending `((unless (single-values-having-validity-p ,value-with-validity-variable)
                                        ,(if (eq action :error)
                                             `(error "Missing value for ~S in interval [~S,~S)" ',variable
                                                     ,validity-start-variable ,validity-end-variable)
                                             `(next-iteration)))
                                      (for ,variable = (single-values-having-validity-value ,value-with-validity-variable))))
                         (collect `(for ,variable = (if (single-values-having-validity-p ,value-with-validity-variable)
                                                        (single-values-having-validity-value ,value-with-validity-variable)
                                                        ,(second action)))))))))))


(def function merge-validities (&rest values-having-validities)
  (iter (with result = (make-array 0 :adjustable t :fill-pointer 0))
        (for values-having-validity :in values-having-validities)
        (flet ((add-validities (validities)
                 (iter (for validity :in-vector validities)
                       (unless (find validity result :test #'local-time=)
                         (vector-push-extend validity result)))))
          (add-validities (validity-starts-of values-having-validity))
          (add-validities (validity-ends-of values-having-validity)))
        (finally
         (sort result #'local-time<)
         (return result))))

(def (macro e) do-values-having-validity (values-having-validities &body forms)
  (with-unique-names (validity-start validity-end)
    `(iter (for ,validity-end :in-vector (merge-validities ,@values-having-validities))
           (for ,validity-start :previous ,validity-end)
           (when ,validity-start
             (with-validity-range ,validity-start ,validity-end
               (bind ,(iter (for values-having-validity :in values-having-validities)
                            (collect `(,values-having-validity
                                       (single-values-having-validity-value
                                        (values-having-validity-value ,values-having-validity *validity-start* *validity-end*)))))
                 ,@forms))))))

(defun normalize-validity (validity)
  (typecase validity
    (local-time validity)
    (string (parse-timestring validity))
    (t (error "Local-time or string expected in COLLECT-VALUE-WITH-VALIDITY, received ~S."
              validity))))

(export 'collect-value-with-validity)

;;; (COLLECT &optional INTO AT RESULT-TYPE)
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
               (vector-push-extend ,(if (typep validity-start-expr 'local-time)
                                        validity-start-expr
                                        `(normalize-validity ,validity-start-expr))
                                   (validity-starts-of ,collect-var))
               (vector-push-extend ,(if (typep validity-end-expr 'local-time)
                                        validity-end-expr
                                        `(normalize-validity ,validity-end-expr))
                                   (validity-ends-of ,collect-var)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; For primitive types

(def function collect-values-having-validity (value-holders value-function validity-start-function validity-end-function no-value-function requested-validity-start requested-validity-end)
  "From a list of ordered (by t) tuples each containing a value, a validity start and a validity end returns the corresponding values-having-validity for the requested range."
  (assert (not (local-time= requested-validity-start requested-validity-end)))
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
                   (when (local-time< validity-start validity-end)
                     (when (< index (length value-holders))
                       (iter (for i :from index :below (length value-holders))
                             (for record = (elt value-holders i))
                             (for record-validity-start = (funcall validity-start-function record))
                             (for record-validity-end = (funcall validity-end-function record))
                             (for merged-validity-start = (local-time-max validity-start record-validity-start))
                             (for merged-validity-end = (local-time-min validity-end record-validity-end))
                             (when (local-time< merged-validity-start merged-validity-end)
                               (push-value-having-validity (funcall value-function record) merged-validity-start merged-validity-end)
                               (%collect-values-having-validity (1+ i) validity-start merged-validity-start)
                               (%collect-values-having-validity (1+ i) merged-validity-end validity-end)
                               (return-from %collect-values-having-validity))))
                     (push-value-having-validity
                      (funcall no-value-function validity-start validity-end)
                      validity-start validity-end))))
          (%collect-values-having-validity 0 requested-validity-start requested-validity-end)
          (sort indices #'local-time< :key (lambda (index) (aref validity-starts index)))
          (permute values indices)
          (permute validity-starts indices)
          (permute validity-ends indices)
          (make-instance 'values-having-validity
                         :values values
                         :validity-starts validity-starts
                         :validity-ends validity-ends)))))

(def function values-having-validity-value (values-having-validity requested-validity-start requested-validity-end)
  "Extracts the requested range if possible, secondary value indicates success."
  (bind ((validity-starts (validity-starts-of values-having-validity))
         (validity-ends (validity-ends-of values-having-validity))
         (first-validity-start (aref validity-starts 0))
         (last-validity-end (aref validity-ends (1- (length validity-ends)))))
    (if (and (local-time<= first-validity-start requested-validity-start)
             (local-time<= requested-validity-end last-validity-end))
        (values
         ;; TODO binary search?
         (iter (for (validity-start validity-end value) :in-values-having-validity values-having-validity)
               ;;(format t "~S ~S~%" validity-start validity-end)
               (cond
                 ((local-time<= validity-end requested-validity-start)
                  nil)
                 ((local-time>= validity-start requested-validity-end)
                  (finish))
                 ((and (local-time<= requested-validity-start validity-start)
                       (local-time<= validity-end requested-validity-end))
                  (collect-value-with-validity value :from validity-start :to validity-end))
                 ((local-time< validity-start requested-validity-start)
                  (collect-value-with-validity value :from requested-validity-start :to validity-end))
                 (t
                  (collect-value-with-validity value :from validity-start :to requested-validity-end))))
         #t)
        (values (make-empty-values-having-validity) #f))))

(def (function e) (setf values-having-validity-value) (new-value values-having-validity validity-start validity-end)
  (bind ((values (make-array 0 :adjustable #t :fill-pointer 0))
         (validity-starts(make-array 0 :adjustable #t :fill-pointer 0))
         (validity-ends (make-array 0 :adjustable #t :fill-pointer 0))
         (addedp #f))
    (flet ((add (value start end)
             (vector-push-extend value values)
             (vector-push-extend start validity-starts)
             (vector-push-extend end validity-ends)))
      (iter (for (start end value) :in-values-having-validity values-having-validity)

            (cond
              ;; |---|        old
              ;;       |---|  new
              ((local-time<= end validity-start)
               (add value start end))
              ;;       |---|  old
              ;; |---|        new
              ((local-time<= validity-end start)
               (unless addedp
                 (add new-value validity-start validity-end)
                 (setf addedp #t))
               (add value start end))
              ;; |----|    old
              ;;   |----|  new
              ((and (local-time< start validity-start)
                    (local-time<= end validity-end))
               (add value start validity-start)
               (add new-value validity-start validity-end)
               (setf addedp #t))
              ;; |--------| old
              ;;   |----|   new
              ((local-time< start validity-start)
               (add value start validity-start)
               (add new-value validity-start validity-end)
               (add value validity-end end)
               (setf addedp #t))
              ;;   |-----| old
              ;; |----|    new
              ((and (local-time<= validity-start start)
                    (local-time< validity-end end))
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

;;;;;;;;;;;;;;;;;
;;; For set types

(def constant +t-delete+ 0
  "Constant used to mark RDBMS records for association slots.")

(def constant +t-insert+ 1
  "Constant used to mark RDBMS records for association slots.")

(def function collect-children-having-validity (value-holders value-function validity-start-function validity-end-function action-function requested-validity-start requested-validity-end)
  "Collect children sets as values-having-validity, value-holders must be ordered by t ascending."
  (labels ((%collect-children-having-validity (value-holders validity-start validity-end)
             (bind ((set nil))
               (iter (for value :in-sequence value-holders)
                     (when (and (local-time<= validity-end (funcall validity-end-function value))
                                (local-time<= (funcall validity-start-function value) validity-start))
                       (ecase (funcall action-function value)
                         (#.+t-insert+
                          (pushnew (funcall value-function value) set))
                         (#.+t-delete+
                          (deletef set (funcall value-function value))))))
               set)))
    (bind ((validities nil))
      (flet ((push-validity (validity)
               (pushnew validity validities :test #'local-time=)))
        (push-validity requested-validity-start)
        (push-validity requested-validity-end)
        (iter (for value :in-sequence value-holders)
              (push-validity (funcall validity-start-function value))
              (push-validity (funcall validity-end-function value))))
      (setf validities (sort validities #'local-time<))
      (if (= 2 (length validities))
          (make-single-values-having-validity (%collect-children-having-validity value-holders (first validities) (second validities)) (first validities) (second validities))
          (bind ((size (1- (length validities)))
                 (values (make-array size :fill-pointer 0))
                 (validity-starts (make-array size :fill-pointer 0))
                 (validity-ends (make-array size :fill-pointer 0)))
            (iter (with value = nil)
                  (with previous-value = :unbound)
                  (with previous-validity = nil)
                  (for validity :in validities)
                  (for modified-validity = validity)
                  (for index :from -1)
                  (if previous-validity
                      (progn
                        (setf value (%collect-children-having-validity value-holders previous-validity modified-validity))
                        (if (equal value previous-value)
                            (decf index)
                            (progn
                              (vector-push value values)
                              (vector-push previous-validity validity-starts)
                              (vector-push modified-validity validity-ends)
                              (setf previous-value value)
                              (setf previous-validity validity))))
                      (setf previous-validity validity)))
            (make-instance 'values-having-validity
                           :values values
                           :validity-starts validity-starts
                           :validity-ends validity-ends))))))

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
;;
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
                        (local-time/= start prev-end)
                        (not (funcall test value prev-value)))))
      values-having-validity ; already consolidated
      (iter (with values = (make-array 0 :adjustable #t :fill-pointer 0))
            (with validity-starts = (make-array 0 :adjustable #t :fill-pointer 0))
            (with validity-ends = (make-array 0 :adjustable #t :fill-pointer 0))
            (for (start end value) :in-values-having-validity values-having-validity)
            (for prev-value :previous value)
            (for prev-end :previous end)
            (unless (and (not (first-iteration-p))
                         (local-time= start prev-end)
                         (funcall test value prev-value))
              (progn
                (when prev-end (vector-push-extend prev-end validity-ends))
                (vector-push-extend value values)
                (vector-push-extend start validity-starts)))
            (finally
             (when end (vector-push-extend end validity-ends))
             (return (make-values-having-validity values validity-starts validity-ends))))))

