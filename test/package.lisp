;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2006 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage :cl-perec-test
    (:nicknames :prct)

  (:use :common-lisp
        :closer-mop
        :iterate
        :anaphora
        :alexandria
        :bind
        :defclass-star
        :computed-class
        :local-time
        :stefil
        :cl-def
        :cl-rdbms
        :cl-perec
        :cl-ppcre
        :metacopy-with-contextl)

  (:shadow #:name
           #:parent)

  (:shadowing-import-from :cl-perec
                          #:time
                          #:form
                          #:set))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import (let ((*package* (find-package :cl-perec)))
            (read-from-string "(*persistent-classes*
                                *persistent-associations* *cache-slot-values*
                                *mapped-type-precedence-list* *canonical-types* *compile-query-counter*
                                *test-query-compiler* +unbound-slot-marker+ +not-cached-slot-marker+
                                unbound-slot-marker-p
                                canonical-type-for normalized-type-for mapped-type-for 
                                first-moment-for-partial-timestamp last-moment-for-partial-timestamp
                                less-or-equal-p greater-or-equal-p
                                validity-begin validity-end t-value unbound-slot-t
                                find-slot collect-if concatenate-symbol
                                unbound-subtype-p null-subtype-p set-type-p set-type-p*
                                invalidate-cached-instance invalidate-all-cached-instances persistent
                                clear-compiled-query-cache reset-compile-query-counter
                                ensure-exported primary-table-slot-p data-table-slot-p
                                primary-table-of primary-tables-of data-tables-of
                                primary-view-of data-view-of
                                prefetch-p cache-p compute-mapping reader-of writer-of
                                compute-rdbms-types compute-reader compute-writer
                                table-of columns-of reader-name-of writer-name-of
                                lisp-value->rdbms-values rdbms-values->lisp-value
                                depends-on-of depends-on-me-of value-having-validity=
                                prc::collect-children-having-validity collect-values-having-validity
                                extract-values-having-validity
                                compile-query random-string)"))
          :cl-perec-test))
