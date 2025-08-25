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

;; Returns three values: the found handler, the class it's found in,
;; and a boolean to signify if a handler was found.
(define (get-handler self messages name)
  (let ((immediate-slot (memq name (messages-alist messages))))
    (if immediate-slot
        (values (cdr immediate-slot) self #t)
        (let loop ((parents (messages-parents messages)))
          (let-values (((handler class found?) (get-handler
                                                (car parents)
                                                ((car parents) private-slot-list)
                                                name)))
            (if found?
                (values handler class found?)
                (if (null? parents)
                    (values #f self #f)
                    (loop (cdr parents)))))))))

(define (messages-handle msg self name args)
  (receive (handler found)
      (get-handler msg self name self args '())
    (run-with-error-checking handler self name args)))

(define (get-handler msg self name receiver args visited)
  (if (memq self visited)
    (values 'message-not-understood #f)
    (cond
     ((messages-direct-lookup msg name)
      => (lambda (handler)
           (values (lambda (handler)
                     (apply handler
                            receiver
                            (make-resender msg self receiver visited)
                            args))
                   self)))
     (else
      (messages-parent-lookup msg self name receiver args visited)))))

(define (messages-direct-lookup msg name)
  (cond ((assq name (messages-alist msg)) => cdr)
        (else #f)))

(define (messages-parent-lookup msg self name receiver args visited)
  (let loop ((alis (messages-parents msg))
             (handler #f)
             (found #f))
    (if (null? alis)
        (if handler
            (values handler found)
            (values 'message-not-understood #f))
        (receive (new new-found)
            (((cdar alis)
              receiver
              (lambda args
                (error "Parent slots must not use resend."
                       receiver name args)))
             '%get-handler name receiver args (cons self visited))
          (case new
            ((message-not-understood) (loop (cdr alis)
                                            handler
                                            found))
            ((ambiguous-message-send) (values 'ambiguous-message-send #f))
            (else (if (and handler (and (not (eq? found new-found))))
                      (values 'ambiguous-message-send #f)
                      (loop (cdr alis) new new-found))))))))

(define (make-resender msg self receiver visited)
  (lambda (target name . args)
    (receive (handler found)
        (cond
         ((eq? target #f) (messages-parent-lookup msg self name receiver args visited))
         ((eq? target self) (get-handler msg self name receiver args visited))
         ((symbol? target) ((self target) '%get-handler name receiver args (cons self visited)))
         (else (target '%get-handler name receiver args (cons self visited))))
      (run-with-error-checking handler self name args))))

;; Signal the appropriate errors, if the handler is not a procedure.
;; Else, call that handler.
(define (run-with-error-checking handler self name args)
  (case handler
    ((message-not-understood) (if (eq? name 'message-not-understood)
                                  (error "Message MESSAGE-NOT-UNDERSTOOD not understood"
                                         self args)
                                  (self 'ambiguous-message-send name args)))
    (else (handler))))

;;;; Core object system

(define (make-core-object)
  (letrec ((msg (make-messages '() '()))
           (self (lambda (name . args)
                   (cond
                    ((eq? name private-slot-list) (messages-alist msg))
                    ((eq? name private-parent-list) (messages-parents msg))
                    (else (messages-handle msg self name args))))))
    self))
