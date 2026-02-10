(import (scheme base)
        (scheme case-lambda)
        (srfi 1)
        (chicken string))

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

(define (delete-slot! message-alist slot-alist parent-list slot)
  (let ((msgs-no-getter (alist-delete! slot message-alist))
        (setter (cond ((assq slot slot-alist)
                       => cadr)
                      (else #f)))
        (value (cond ((assq slot slot-alist)
                      => cadddr)
                     (else #f))))
    (values
     (alist-delete! setter msgs-no-getter)
     (alist-delete! slot slot-alist)
     (lset-xor eq? parent-list (list value)))))

;;; Setters aren't created properly. They should show up as a message.
(define (add-slot! type message-alist slot-alist value-store-alist parent-list slot . args)
  (let* ((setter? (< 1 (length args)))
         (setter-name (and setter? (car args)))
         (value (if setter? (cadr args) (car args)))
         (getter (if (eq? type 'method)
                     value
                     (lambda (self resend)   ; wrap the value in value slots 
                       (let-values
                           (((val found?) ; for more streamlined access
                             (recursive-method-lookup self (slot-value-finder slot)
                                                      #f)))
                         val))))
         (setter (lambda (self resend new-value)
                   (when (eq? type 'parent)
                     (set! parent-list
                       (lset-adjoin eq?
                                    (lset-xor eq? parent-list (list value))
                                    new-value)))
                   (self '--set-value-store slot new-value)))
         (msgs-with-getter (alist-set! message-alist slot getter))
         (msgs-with-setter (alist-set! msgs-with-getter setter-name setter)))
    (values                                         ; returning...
     (if setter? msgs-with-setter msgs-with-getter) ; message-alist
     (alist-set! slot-alist slot                    ; slot-alist
                 (list slot setter-name type value))
     (if (eq? type 'value) (alist-set! value-store-alist slot value)
         value-store-alist)             ; value-store-alist
     (if (eq? type 'parent)             ; parent-list
         (lset-adjoin eq? parent-list value) parent-list))))

(define (method-finder name message-alist)
  (lambda (self)
    (if (eqv? name 'mirror)
        (assq name message-alist)
        (assq name ((self 'mirror) 'immediate-message-alist)))))

(define (slot-value-finder name)
  (lambda (self)
    (assq name ((self 'mirror) 'value-store-alist))))

(define (recursive-method-lookup self checker skip?)
  (cond
   ((and (not skip?) (checker self))
    => (lambda (alist-entry)
         (values (cdr alist-entry) #t)))
   (else
    (let ((mirror (self 'mirror)))
      (let loop ((parents (mirror 'immediate-ancestor-list))
                 (handler-count 0)
                 (handler #f)
                 (found #f))
        (cond
         ((not (null? parents))
          (let-values (((new-handler new-found)
                        (recursive-method-lookup (car parents) checker #f)))
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
        (lambda (message-alist slot-alist value-store-alist parent-list)
          (letrec
              ((obj-handler
                (lambda (command . args)
                  (if (eqv? command '--set-value-store)
                      (set! value-store-alist
                        (alist-set! value-store-alist (car args) (cadr args)))
                      (send-with-error-handling
                       obj-handler command message-alist #f args))))
               (add-*-slot!
                (lambda (type)
                  (lambda (self resend name . args)
                    (let-values
                        (((new-msgs new-slots new-value-store-alist new-parents)
                          (apply add-slot! type
                                 message-alist slot-alist value-store-alist parent-list name args)))
                      (set! message-alist new-msgs)
                      (set! slot-alist new-slots)
                      (set! value-store-alist new-value-store-alist)
                      (set! parent-list new-parents))))))
            ((add-*-slot! 'method)
             #f #f 'mirror (lambda (self resend)
                             (lambda (message . args)
                               (case message
                                 ((immediate-ancestor-list) parent-list)
                                 ((full-ancestor-list) (recursive-ancestor-collector self))
                                 ((immediate-message-alist) message-alist)
                                 ((immediate-slot-list) (map cdr slot-alist))
                                 ((value-store-alist) value-store-alist)
                                 (else
                                  (error "Message not understood:" message))))))
            ((add-*-slot! 'method)
             #f #f 'clone (lambda (self resend)
                            (let-values
                                (((message-alist slot-alist value-store-alist parent-list)
                                  (add-slot! 'parent '() '() '() '() 'parent self)))
                              (object message-alist slot-alist value-store-alist parent-list))))
            ((add-*-slot! 'method)
             #f #f 'delete-slot! (lambda (self resend name)
                                   (let-values
                                       (((new-msgs new-slots new-parents)
                                         (delete-slot! message-alist slot-alist parent-list name)))
                                     (set! message-alist new-msgs)
                                     (set! slot-alist new-slots)
                                     (set! parent-list new-parents))))
            ((add-*-slot! 'method) #f #f 'add-value-slot! (add-*-slot! 'value))
            ((add-*-slot! 'method) #f #f 'add-method-slot! (add-*-slot! 'method))
            ((add-*-slot! 'method) #f #f 'add-parent-slot! (add-*-slot! 'parent))
            obj-handler))))
    (object '() '() '() '())))

;;;; Method running

(define (send-with-error-handling self method-name message-alist parents-only args)
  (let-values (((method found?)
                (recursive-method-lookup
                 self
                 (method-finder method-name message-alist)
                 parents-only)))
    (if found?
        (apply method self (make-resender self method-name) args)
        (case method
          ((message-not-understood)
           (error "Message not understood" self args))
          ((ambiguous-message-send)
           (error "Ambiguous message send" self args))))))

(define (make-resender self handler-name)
  (lambda (target-override . args)
    (let ((target (cond
                   ((eqv? #f target-override)
                    self)
                   (else target-override))))
      (send-with-error-handling target handler-name '() (eq? target-override #f) args))))
