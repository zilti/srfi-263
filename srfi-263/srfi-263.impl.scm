(import (scheme base)
        (scheme case-lambda)
        (srfi 1))

;;; Helpers

(define (alist-set alist name value)
  (cond
   ((assq name alist)
    => (lambda (entry)
         (set-cdr! entry value)
         alist))
   (else
    (alist-cons name value alist))))

;;; Core system

(define (delete-slot! message-alist slot-list parent-list slot)
  (let* ((slot-predicate (lambda (item)
                           (or (eq? (car item) slot)
                              (eq? (cadr item) slot))))
         (slots (filter slot-predicate slot-list)))
    (if (= 1 (length slots))
        (let ((slot (car slots)))
          (values
           (alist-delete (car slot) (alist-delete (cadr slot) message-alist))
           (remove slot-predicate slot-list)
           (if (eq? 'parent (caddr slot))
               (delete (car slot) parent-list)
               parent-list)))
        (values message-alist slot-list parent-list))))

(define (add-value-slot! message-alist slot-list slot . args)
  (let-values (((message-alist slot-list _)
                (delete-slot! message-alist slot-list '() slot)))
    (let* ((setter? (< 1 (length args)))
           (setter-name (and setter? (car args)))
           (value (if setter? (cadr args) (car args)))
           (getter (lambda (self resend)
                     value))
           (setter (lambda (self resend . args)
                     (apply self 'add-value-slot! slot
                            (if setter? (cons setter-name args)
                                args))))
           (message-alist (append (if setter?
                                      `((,slot . ,getter)
                                        (,setter-name . ,setter))
                                      `((,slot . ,getter)))
                                  message-alist))
           (slot-list (append `((,slot ,setter-name value))
                              slot-list)))
      (values
       message-alist slot-list))))

(define (add-method-slot! message-alist slot-list slot proc)
  (let-values (((message-alist slot-list _)
                (delete-slot! message-alist slot-list '() slot)))
    (values
     (alist-set message-alist slot proc)
     (cons `(,slot #f method) slot-list))))

(define (add-parent-slot! message-alist slot-list parent-list slot parent)
  (let-values (((message-alist slot-list parent-list)
                (delete-slot! message-alist slot-list parent-list slot)))
    (let ((parent-slot `((,slot #f parent))))
      (values
       (alist-set message-alist slot parent)
       (append parent-slot slot-list)
       (cons parent parent-list)))))

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
               parents
               (map recursive-ancestor-collector parents)))))

(define (recursive-slot-collector self)
  (let ((parents (recursive-ancestor-collector self)))
    (apply lset-union
           (lambda (a b)
             (eq? (car a) (car b)))
           (list)
           (map (lambda (class)
                  ((class 'mirror) 'immediate-slot-list))
                parents))))

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

;;;; Mirror

(define (mirror self message-alist slot-list parent-list)
  (lambda (message . args)
    (case message
      ((immediate-message-alist) message-alist)
      ((immediate-ancestor-list) parent-list)
      ((full-ancestor-list) (recursive-ancestor-collector self))
      ((immediate-slot-list) slot-list)
      ((full-slot-list) (recursive-slot-collector self))
      (else
       (error "Message not understood" message)))))

;;;; Root object

(define *the-root-object*
  (letrec
      ((object
        (lambda (message-alist slot-list parent-list)
          (letrec
              ((obj-handler
                (lambda (message . args)
                  (if (eq? message 'mirror)
                      (mirror obj-handler message-alist slot-list parent-list)
                      (send-with-error-handling
                       obj-handler obj-handler message message-alist #f args)))))
            (set! message-alist
              (alist-cons
               'add-method-slot!
               (lambda (self resend name proc)
                 (let-values
                     (((new-message-alist new-slot-list)
                       (add-method-slot! message-alist slot-list name proc)))
                   (set! message-alist new-message-alist)
                   (set! slot-list new-slot-list)))
               message-alist))
            (set! slot-list (append `((add-method-slot! #f method)) slot-list))
            (obj-handler 'add-method-slot! 'mirror
                         (lambda (self resend)
                           (mirror self message-alist slot-list parent-list)))
            (obj-handler 'add-method-slot! 'clone
                         (lambda (self resend)
                           (let-values
                               (((message-alist slot-list parent-list)
                                 (add-parent-slot! '() '() '() 'parent self)))
                             (object message-alist slot-list parent-list))))
            (obj-handler 'add-method-slot! 'delete-slot!
                         (lambda (self resend name)
                           (let-values
                               (((new-message-alist new-slot-list new-parent-list)
                                 (delete-slot! message-alist slot-list parent-list name)))
                             (set! message-alist new-message-alist)
                             (set! slot-list new-slot-list)
                             (set! parent-list new-parent-list))))
            (obj-handler 'add-method-slot! 'add-value-slot!
                         (lambda (self resend name . args)
                           (let-values (((new-message-alist new-slot-list)
                                         (apply add-value-slot! message-alist slot-list name args)))
                             (set! message-alist new-message-alist)
                             (set! slot-list new-slot-list))))
            (obj-handler 'add-method-slot! 'add-parent-slot!
                         (lambda (self resend name parent)
                           (let-values (((new-message-alist new-slot-list new-parent-list)
                                         (add-parent-slot! message-alist slot-list parent-list name parent)))
                             (set! message-alist new-message-alist)
                             (set! slot-list new-slot-list)
                             (set! parent-list new-parent-list))))
            obj-handler))))
    (object '() '() '())))
