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

(define (delete-slot! message-alist parent-list slot)
  (let ((msgs-no-getter (alist-delete! slot message-alist)))
    (values
     (alist-delete! setter msgs-no-getter)
     (lset-xor eq? parent-list (list slot)))))

(define (add-value-slot! self resend slot . args)
  (let* ((setter? (< 1 (length args)))
         (setter-name (and setter? (car args)))
         (value (if setter? (cadr args) (car args)))
         (getter (lambda (self2 resend)
                   value))
         (setter (lambda (self2 resend . args)
                   (apply add-value-slot! self2 #f slot
                                    (if setter? (cons setter-name args)
                                        args)))))
    (self 'delete-slot! slot)
    (self 'add-method-slot! slot getter)
    (when setter?
      (self 'delete-slot! setter-name)
      (self 'add-method-slot! setter-name setter))))

(define (add-method-slot! message-alist slot proc)
  (alist-set! message-alist slot proc))

(define (add-parent-slot! message-alist parent-list slot . args)
  (values
   (alist-set! message-alist slot (car args))
   (lset-adjoin eq?
                (lset-xor eq? parent-list args))))

(define (method-finder name message-alist)
  (lambda (self)
    (cond ((if (eqv? name 'mirror)
                (assq name message-alist)
                (assq name ((self 'mirror) 'immediate-message-alist)))
           => cdr)
          (else #f))))

(define (recursive-lookup self checker skip?)
  (cond
   ((and (not skip?) (checker self))
    => (lambda (alist-entry)
         (values alist-entry #t)))
   (else
    (let ((mirror (self 'mirror)))
      (let loop ((parents (mirror 'immediate-ancestor-list))
                 (handler-count 0)
                 (handler #f)
                 (found #f))
        (cond
         ((not (null? parents))
          (let-values (((new-handler new-found)
                        (recursive-lookup (car parents) checker #f)))
            (loop (cdr parents)
                  (if new-found (add1 handler-count) handler-count)
                  (if new-found new-handler handler)
                  (or new-found found))))
         (else
          (if handler
              (if (= 1 handler-count)
                  (values handler found)
                  (values 'ambiguous-message-send #f))
              (values 'message-not-understood #f)))))))))

(define (recursive-ancestor-collector self)
  (let ((parents ((self 'mirror) 'immediate-ancestor-list)))
    (if (null? parents)
        (list self)
        (apply lset-union
               eq?
               (list self)
               parents
               (map recursive-ancestor-collector parents)))))

(define *the-root-object*
  (letrec
      ((object
        (lambda (message-alist parent-list)
          (letrec
              ((obj-handler
                (lambda (command . args)
                  (send-with-error-handling
                   obj-handler obj-handler command message-alist #f args))))
            (set! message-alist
              (alist-cons
               'mirror
               (lambda (self resend)
                 (lambda (message . args)
                   (case message
                     ((immediate-ancestor-list) parent-list)
                     ((full-ancestor-list) (recursive-ancestor-collector self))
                     ((immediate-message-alist) message-alist)
                     (else
                      (error "Message not understood:" message)))))
               message-alist))
            (set! message-alist
              (alist-cons
               'clone
               (lambda (self resend)
                 (let-values
                     (((message-alist parent-list)
                       (add-parent-slot! '() '() 'parent self)))
                   (object message-alist parent-list)))
               message-alist))
            (set! message-alist
              (alist-cons
               'delete-slot!
               (lambda (self resend name)
                 (let-values
                     (((new-msgs new-parents)
                       (delete-slot! message-alist parent-list name)))
                   (set! message-alist new-msgs)
                   (set! parent-list new-parents)))
               message-alist))
            (set! message-alist
              (alist-cons
               'add-method-slot!
               (lambda (self resend name proc)
                 (set! message-alist
                   (add-method-slot! message-alist name proc)))
               message-alist))
            (set! message-alist
              (alist-cons
               'add-value-slot!
               add-value-slot!
               message-alist))
            (set! message-alist
              (alist-cons
               'add-parent-slot!
               (lambda (self resend name parent)
                 (let-values (((new-message-alist new-parent-list)
                               (add-parent-slot! message-alist parent-list name parent)))
                   (set! message-alist new-message-alist)
                   (set! parent-list new-parent-list)))
               message-alist))
            obj-handler))))
    (object '() '())))

;;;; Method running

(define (send-with-error-handling caller method-lookup method-name message-alist parents-only args)
  (let-values (((method found?)
                (recursive-lookup
                 method-lookup
                 (method-finder method-name message-alist)
                 parents-only)))
    (if found?
        (apply method caller (make-resender caller method-name) args)
        (case method
          ((message-not-understood)
           (error "Message not understood" caller args))
          ((ambiguous-message-send)
           (error "Ambiguous message send" caller args))))))

(define (make-resender caller handler-name)
  (lambda (target-override . args)
    (let ((target (cond
                   ((eqv? #f target-override)
                    caller)
                   (else target-override))))
      (send-with-error-handling caller target handler-name '() (eq? target-override #f) args))))
