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
        :arnesi
        :bind
        :defclass-star
        :computed-class
        :local-time
        :stefil
        :cl-rdbms
        :cl-perec
        :cl-ppcre
        :metacopy-with-contextl)

  (:shadow #:name
           #:parent)

  (:shadowing-import-from :cl-perec
                          #:time
                          #:form
                          #:set
                          #:sql-fragment))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import (let ((*package* (find-package :cl-perec)))
            (read-from-string "(*persistent-classes*
                                *persistent-associations* *cache-slot-values*
                                *mapped-type-precedence-list* *canonical-types* *compile-query-counter*
                                *test-query-compiler* +unbound-slot-marker+ +not-cached-slot-marker+
                                elt-0-0 elt-0 elt-1
                                canonical-type-for normalized-type-for mapped-type-for 
                                date-of-first-day-for-partial-date date-of-last-day-for-partial-date
                                less-or-equal-p greater-or-equal-p
                                validity-start validity-end t-value unbound-slot-t
                                find-slot length=1 collect-if concatenate-symbol
                                null-inclusive-type-p unbound-subtype-p set-type-p
                                invalidate-all-cached-slots persistent
                                clear-compiled-query-cache reset-compile-query-counter
                                ensure-exported primary-table-slot-p data-table-slot-p
                                primary-table-of primary-tables-of data-tables-of
                                prefetch-p cache-p compute-writer compute-reader
                                table-of columns-of reader-name-of writer-name-of
                                depends-on-of depends-on-me-of
                                extract-values-having-validity-range
                                compile-query)"))
          :cl-perec-test))
