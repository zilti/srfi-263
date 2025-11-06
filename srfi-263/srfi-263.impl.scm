(import (scheme base)
        (scheme case-lambda)
        (srfi 1))

;;; Helpers

(define (alist-set! alist name value)
  (cond
   ((assq name alist)
    => (lambda (entry)
         (set-cdr! entry value)
         alist))
   (else
    (alist-cons name value alist))))

;;; Core system

;;; Private message to get the internal message and parent list
(define private-slot-list '(private slot list))
(define private-parent-list '(private parent list))

(define-record-type messages
  (make-messages alist parents)
  messages?
  (alist messages-alist set-messages-alist!)
  (parents messages-parents set-messages-parents!))

(define-record-type message
  (make-message type getter setter value)
  message?
  (type message-type)
  (getter message-getter)
  (setter message-setter)
  (value message-value set-message-value!))

(define (messages-add! messages message)
  (set-messages-alist! messages
                       (alist-set!
                        (messages-alist messages)
                        (message-getter message) message))
  (when (message-setter message)
    (set-messages-alist! messages
                         (alist-set!
                          (messages-alist messages)
                          (message-setter message) message)))
  (when (eq? 'parent (message-type message))
    (set-messages-parents! messages
                           (alist-set!
                            (messages-parents messages)
                            (message-getter message) (message-value message)))))

(define (messages-delete! messages name)
  (set-messages-alist! messages
                       (alist-delete! name
                                      (messages-alist messages)
                                      eq?))
  (set-messages-parents! messages
                         (alist-delete! name
                                        (messages-parents messages)
                                        eq?)))

(define (messages-handle messages self name args)
  (receive (handler found)
      (get-handler messages self name self args '())
    (run-with-error-checking handler self name args)))

(define (messages-direct-lookup messages name)
  (cond ((assq name (messages-alist messages)) => cdr)
        (else #f)))

(define (messages-recursive-lookup messages name)
  (cond
   ((assq name (messages-alist messages))
    => (lambda (messages-entry)
         (values messages (cdr messages-entry) #t)))
   (else
    (let loop ((alis (messages-parents messages))
               (handler-count 0)
               (receiver #f)
               (handler #f)
               (found #f))
      (if (null? alis)
          (if handler
              (if (= 1 handler-count)
                  (values receiver handler found)
                  (values #f 'ambiguous-message-send #f))
              (values #f 'message-not-understood #f))
          (let-values (((new-receiver new-handler new-found)
                        (messages-recursive-lookup (car alis) name)))
            (loop (cdr alis)
                  (if new-found (add1 handler-count) handler-count)
                  (if new-found new-receiver receiver)
                  (if new-found new-handler handler)
                  (or new-found found))))))))

;;;; Core object system

(define (create-object)
  (letrec ((msg (make-messages '() '()))
           (self (lambda (name . args)
                   (cond
                    ((eq? name 'private-msgs) msg)
                    (else (messages-handle msg self name args))))))
    self))
