(import (scheme base)
        (srfi 263))

(define-syntax set-method!
  (syntax-rules ()
    ((_ (obj message self resend args ...)
        body1 body ...)
     (obj 'set-method-slot! `message
          (lambda (self resend args ...)
            body1 body ...)))))

(define-syntax define-object
  (syntax-rules ()
    ((_ name (creation-parent (parent-name parent-object) ...)
        slots ...)
     (define name (let ((o (creation-parent 'clone)))
                    (o 'set-parent-slot! 'parent-name parent-object)
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
       (o 'set-method-slot! `method-name (lambda method-args
                                           body ...))
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-setter slot-value)
        slots ...)
     (begin
       (o 'set-value-slot! `slot-getter `slot-setter slot-value)
       (define-object/add-slots! o slots ...)))
    ((_ o (slot-getter slot-value)
        slots ...)
     (begin
       (o 'set-value-slot! `slot-getter slot-value)
       (define-object/add-slots! o slots ...)))))
