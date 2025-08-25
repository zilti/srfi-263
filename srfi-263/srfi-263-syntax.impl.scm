(import (scheme base)
        (srfi 263))

(define-syntax define-method
  (syntax-rules ()
    ((_ (obj `message self resend args ...)
        body1 body)
     (obj 'add-method-slot! `message
          (lambda (self resend args ...)
            body1 body ...)))))

(define-syntax define-object
  (syntax-rules ()
    ((_ name (creation-parent (parent-name parent-object) ...)
        slots ...)
     (define name (let ((o (creation-parent 'clone)))
                    (o 'add-parent-slot! 'parent-name parent-object)
                    ...
                    (define-object/add-slots! o slots ...)
                    o)))))

(define-syntax define-object/add-slots!
  (syntax-rules ()
    ((_ o)
     (void))
    ((_ o ((method-name . method-args) body ...)
        slots ...)
     (begin
       (o 'add-method-slot! `method-name (lambda method-args
                                           body ...))
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-setter slot-value)
        slots ...)
     (begin
       (o 'add-value-slot! `slot-getter `slot-setter slot-value)
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-value)
        slots ...)
     (begin
       (o 'add-value-slot! `slot-getter slot-value)
       (define-object/add-slots! o slots ...)))))
