(in-package :cl-perec)

(define-copy-protocol copy-query)

(define-copy-method (copy-one copy-query) ((class persistent-class) htable)
  class)

(define-copy-method (copy-one copy-query) ((object persistent-object) htable)
  object)

(define-copy-method (copy-one copy-query) ((local-time local-time) htable)
  local-time)

(define-copy-method (copy-one copy-query) ((slot persistent-slot-definition) htable)
  slot)
