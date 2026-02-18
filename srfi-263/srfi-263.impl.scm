(import (scheme base)
        (scheme case-lambda)
        (scheme cxr)
        (scheme write)
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

(define (delete-slot! obj-data slot)
  (let* ((message-alist (get-message-alist obj-data))
         (slot-list (get-slot-list obj-data))
         (parent-list (get-parent-list obj-data))
         (slot-predicate (lambda (item)
                           (or (eq? (car item) slot)
                              (eq? (cadr item) slot))))
         (slots (filter slot-predicate slot-list)))
    (if (= 1 (length slots))
        (let ((slot (car slots)))
          (set-message-alist! obj-data
                              (alist-delete (car slot)
                                            (alist-delete (cadr slot) message-alist)))
          (set-slot-list! obj-data (remove slot-predicate slot-list))
          (if (eq? 'parent (caddr slot))
              (set-parent-list! (delete (car slot) parent-list)))))))

(define (add-value-slot! obj-data slot . args)
  (delete-slot! obj-data slot)
  (let* ((setter? (< 1 (length args)))
         (setter-name (and setter? (car args)))
         (value (if setter? (cadr args) (car args)))
         (getter (lambda (self resend)
                   value))
         (setter (lambda (self resend . args)
                   (apply self 'add-value-slot! slot
                          (if setter? (cons setter-name args)
                              args)))))
    (set-message-alist! obj-data
                        (append (if setter?
                                    `((,slot . ,getter)
                                      (,setter-name . ,setter))
                                    `((,slot . ,getter)))
                                (get-message-alist obj-data)))
    (set-slot-list! obj-data
                    (append `((,slot ,setter-name value))
                            (get-slot-list obj-data)))))

(define (add-method-slot! obj-data slot proc)
  (delete-slot! obj-data slot)
  (set-message-alist! obj-data
                      (alist-set (get-message-alist obj-data)
                                 slot proc))
  (set-slot-list! obj-data
                  (cons `(,slot #f method)
                        (get-slot-list obj-data))))

(define (add-parent-slot! obj-data slot parent)
  (delete-slot! obj-data slot)
  (set-message-alist! obj-data (alist-set (get-message-alist obj-data) slot parent))
  (set-slot-list! obj-data (append `((,slot #f parent)) (get-slot-list obj-data)))
  (set-parent-list! obj-data (cons parent (get-parent-list obj-data))))

(define (method-finder name message-alist)
  (lambda (self)
    (cond ((if (eq? name 'mirror)
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
                  (if new-found (+ handler-count 1) handler-count)
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
    (apply method caller (make-resender caller method-name) args)))

(define (make-resender caller handler-name)
  (lambda (target-override . args)
    (let ((target (cond
                   ((eq? #f target-override)
                    caller)
                   (else target-override))))
      (send-with-error-handling caller target handler-name '() (eq? target-override #f) args))))

;;;; Mirror

(define (mirror self obj-data)
  (lambda (message . args)
    (case message
      ((--object-data) obj-data)
      ((immediate-message-alist) (get-message-alist obj-data))
      ((immediate-ancestor-list) (get-parent-list obj-data))
      ((full-ancestor-list) (recursive-ancestor-collector self))
      ((immediate-slot-list) (get-slot-list obj-data))
      ((full-slot-list) (recursive-slot-collector self))
      (else
       (error "Message not understood" message)))))

;;;; Root object

(define-record-type object-data
  (make-object-data* message-alist slot-list parent-list)
  object-data?
  (message-alist get-message-alist set-message-alist!)
  (slot-list get-slot-list set-slot-list!)
  (parent-list get-parent-list set-parent-list!))

(define (make-object-data)
  (make-object-data* '() '() '()))

(define *maybe-the-root-object*
  (let ((data (make-object-data)))
    #f))

(define (*object* obj-data)
  (letrec
      ((obj-handler
        (lambda (message . args)
          (if (eq? message 'mirror)
              (mirror obj-handler obj-data)
              (send-with-error-handling
               obj-handler obj-handler message (get-message-alist obj-data) #f args)))))
    obj-handler))

(define *the-root-object*
  (let* ((object (*object* (make-object-data)))
         (mirror (object 'mirror))
         (obj-data (mirror '--object-data)))
    (set-message-alist!
     obj-data
     (alist-cons
      'add-method-slot!
      (lambda (self resend name proc)
        (add-method-slot! ((self 'mirror) '--object-data) name proc))
      (get-message-alist obj-data)))
    (set-slot-list!
     obj-data
     (append `((add-method-slot! #f method)) (get-slot-list obj-data)))
    (object 'add-method-slot! 'mirror
            (lambda (self resend)
              (mirror self obj-data)))
    (object 'add-method-slot! 'clone
            (lambda (self resend)
              (let ((cloned-object (*object* (make-object-data))))
                (add-parent-slot! ((cloned-object 'mirror) '--object-data)
                                  'parent self)
                cloned-object)))
    (object 'add-method-slot! 'delete-slot!
            (lambda (self resend name)
              (delete-slot! ((self 'mirror) '--object-data) name)))
    (object 'add-method-slot! 'add-value-slot!
            (lambda (self resend name . args)
              (apply add-value-slot! ((self 'mirror) '--object-data) name args)))
    (object 'add-method-slot! 'add-parent-slot!
            (lambda (self resend name parent)
              (add-parent-slot! ((self 'mirror) '--object-data) name parent)))
    (object 'add-method-slot! 'message-not-understood
            (lambda (self resend message args)
              (error "Message not understood" self message args)))
    (object 'add-method-slot! 'ambiguous-message-send
            (lambda (self resend message args)
              (error "Message ambiguous" self message args)))
    object))
