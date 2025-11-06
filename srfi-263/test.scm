(include "srfi-263.impl.scm")

(define testmethod
  (lambda () 'success))

;;; Simple

;; Check if method is found on first level
(define test-msgs
  (make-messages '() '()))

(messages-add! test-msgs
               (make-message 'method 'test-method #f testmethod))


(let-values (((receiver handler found)
              (messages-recursive-lookup test-msgs 'test-method)))
  (assert found)
  (assert (eq? test-msgs receiver)))

;;; Simple Inheritance

;; Check if method is found on parent
(define test-msgs-parent
  (make-messages '() '()))

(define test-msgs
  (make-messages '() `(,test-msgs-parent)))

(messages-add! test-msgs-parent
               (make-message 'method 'test-method #f testmethod))

(let-values (((receiver handler found)
              (messages-recursive-lookup test-msgs 'test-method)))
  (assert found)
  (assert (eq? test-msgs-parent receiver)))

;; Check if method is found on child if it is on both
(messages-add! test-msgs
               (make-message 'method 'test-method #f testmethod))

(let-values (((receiver handler found)
              (messages-recursive-lookup test-msgs 'test-method)))
  (assert found)
  (assert (eq? test-msgs receiver)))

;; Check if procedure properly returns if searched method does not exist
(let-values (((receiver handler found)
              (messages-recursive-lookup test-msgs 'not-existing-method)))
  (assert (not found))
  (assert (eq? handler 'message-not-understood)))

;;; Multi-parent Inheritance
(define test-msgs-mother
  (make-messages '() '()))

(define test-msgs-father
  (make-messages '() '()))

(define test-msgs
  (make-messages '() `(,test-msgs-mother ,test-msgs-father)))

(messages-add! test-msgs-mother
               (make-message 'method 'test-method #f testmethod))
(messages-add! test-msgs-father
               (make-message 'method 'test-method #f testmethod))

(let-values (((receiver handler found)
              (messages-recursive-lookup test-msgs 'test-method)))
  (assert (not found))
  (assert (eq? handler 'ambiguous-message-send)))
