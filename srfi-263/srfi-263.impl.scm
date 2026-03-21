(import (scheme base)
        (scheme case-lambda)
        (scheme cxr)
        (scheme write)
        (srfi 1)
        (trace))

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
         (setter-predicate (lambda (item) (eq? (cadr item) slot)))
         (is-setter? (find setter-predicate slot-list))
         (slot-predicate (lambda (item)
                           (or (eq? (car item) slot)
                              (eq? (cadr item) slot))))
         (slots (filter slot-predicate slot-list)))
    (if (= 1 (length slots))
        (let ((slot (car slots)))
          (set-message-alist!
           obj-data
           (if is-setter?
               (alist-delete (cadr slot) message-alist)
               (alist-delete (car slot)
                             (alist-delete (cadr slot) message-alist))))
          (set-slot-list!
           obj-data
           (if is-setter?
               (map (lambda (item)
                      (if (setter-predicate item)
                          `(,(car item) #f ,(caddr item))
                          item))
                    slot-list)
               (remove slot-predicate slot-list)))
          (if (eq? 'parent (caddr slot))
              (set-parent-list! (delete (car slot) parent-list)))))))

(define (slot-add-message-name type)
  (case type
    ((value) 'set-value-slot!)
    ((method) 'set-method-slot!)
    ((parent) 'set-parent-slot!)))

(define (gen-accessors type getter-name setter-name value)
  (values
   (case type
     ((value) (lambda (self resend) value))
     ((method) value)
     ((parent) (lambda (self resend) value)))
   (if setter-name
       (lambda (self resend value)
         (apply self (slot-add-message-name type) getter-name
                (if setter-name (list setter-name value) value)))
       #f)))

(define (set-object-data-slots! obj-data type getter-name getter setter-name setter)
  (let ((new-messages (if setter
                          `((,getter-name . ,getter)
                            (,setter-name . ,setter))
                          `((,getter-name . ,getter)))))
    (set-message-alist!
     obj-data (append new-messages (get-message-alist obj-data)))
    (set-slot-list!
     obj-data (cons `(,getter-name ,setter-name ,type) (get-slot-list obj-data)))))

(define (set-slot! obj-data type getter-name . args)
  (let* ((setter? (< 1 (length args)))
         (setter-name (and setter? (car args)))
         (value (if setter? (cadr args) (car args))))
    (let-values (((getter setter)
                  (gen-accessors type getter-name setter-name value)))
      (delete-slot! obj-data getter-name)
      (set-object-data-slots! obj-data type getter-name getter setter-name setter)
      (when (eq? type 'parent)
        (set-parent-list!
         obj-data (cons value (get-parent-list obj-data)))))))

(define (method-finder name message-alist)
  (letrec ((mfinder
            (lambda (self)
              (cond ((or
                      (assq name message-alist)
                      (assq name (get-message-alist ((self 'mirror) '--obj-data))))
                     => cdr)
                    (else #f)))))
    mfinder))

(define (recursive-lookup self checker skip?)
  (cond
   ((and (not skip?) (checker self))
    => (lambda (alist-entry)
         (values alist-entry #t)))
   (else
    (let ((obj-data ((self 'mirror) '--obj-data)))
      (let loop ((parents (get-parent-list obj-data))
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
  (let ((parents (get-parent-list ((self 'mirror) '--obj-data))))
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
                  (get-slot-list ((class 'mirror) '--obj-data)))
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

;;;; Root object

(define-record-type object-data
  (make-object-data* message-alist slot-list parent-list)
  object-data?
  (message-alist get-message-alist set-message-alist!)
  (slot-list get-slot-list set-slot-list!)
  (parent-list get-parent-list set-parent-list!))

(define (make-object-data)
  (make-object-data* '() '() '()))

(define (*object* obj-data)
  (letrec
      ((obj-handler
        (lambda (message . args)
          (send-with-error-handling
           obj-handler obj-handler message (get-message-alist obj-data) #f args))))
    obj-handler))

(define (set-method-slot! obj-data name . args)
  (apply set-slot! obj-data 'method name args))

(define (derive-object obj mirror?)
  (let* ((obj-data (make-object-data))
         (derived-object (*object* obj-data)))
    (set-slot! obj-data 'parent 'parent obj)
    (set-method-slot!
     obj-data 'mirror
     (lambda (self resend)
       (let-values (((new-mirror new-mirror-data)
                     (derive-object (obj 'mirror) #t)))
         (populate-mirror new-mirror new-mirror-data obj-data))))
    (when mirror?
      (set-method-slot! obj-data 'derive
                        (lambda (self resend)
                          (let-values (((new-obj new-data)
                                        (derive-object self #t)))
                            new-obj))))
    (values derived-object obj-data)))

(define (populate-mirror mirror mirror-data obj-data)
  (map
   (lambda (name proc)
     (set-method-slot! mirror-data name proc))
   '(--obj-data immediate-message-alist
                immediate-ancestor-list full-ancestor-list
                immediate-slot-list full-slot-list)
   (list (lambda (self resend) obj-data)
         (lambda (self resend) (list-copy (get-message-alist obj-data)))
         (lambda (self resend) (list-copy (get-parent-list obj-data)))
         (lambda (self resend) (recursive-ancestor-collector self))
         (lambda (self resend) (list-copy (get-slot-list obj-data)))
         (lambda (self resend) (recursive-slot-collector self))))
  mirror)

(define *the-root-object*
  (let* ((obj-data (make-object-data))
         (object (*object* obj-data)))
    (set-message-alist!
     obj-data
     (alist-cons 'set-method-slot!
                 (lambda (self resend name . args)
                   (apply set-method-slot! ((self 'mirror) '--obj-data)
                          name args))
                 (get-message-alist obj-data)))
    (set-slot-list!
     obj-data
     (append `((set-method-slot! #f method)) (get-slot-list obj-data)))
    (set-method-slot!
     obj-data 'mirror
     (lambda (self resend)
       (let-values (((root-mirror mirror-data) (derive-object *the-root-object* #t)))
         (populate-mirror root-mirror mirror-data obj-data))))
    (set-method-slot!
     obj-data 'derive
     (lambda (self resend)
       (derive-object self #f)))
    (set-method-slot!
     obj-data 'copy
     (lambda (self resend)
       (let ((mirror (self 'mirror))
             (obj-data (make-object-data)))
         (set-message-alist! obj-data (list-copy (mirror 'get-message-alist)))
         (set-slot-list! obj-data (list-copy (mirror 'get-slot-list)))
         (set-parent-list! obj-data (list-copy (mirror 'get-parent-list)))
         (*object* obj-data))))
    (set-method-slot!
     obj-data 'delete-slot!
     (lambda (self resend name)
       (delete-slot! ((self 'mirror) '--obj-data) name)))
    (set-method-slot!
     obj-data 'set-value-slot!
     (lambda (self resend name . args)
       (apply set-slot! ((self 'mirror) '--obj-data) 'value name args)))
    (set-method-slot!
     obj-data 'set-parent-slot!
     (lambda (self resend name . args)
       (apply set-slot! ((self 'mirror) '--obj-data) 'parent name args)))
    (set-method-slot!
     obj-data 'message-not-understood
     (lambda (self resend message args)
       (error "Message not understood" self message args)))
    (set-method-slot!
     obj-data 'ambiguous-message-send
     (lambda (self resend message args)
       (error "Message ambiguous" self message args)))
    object))
