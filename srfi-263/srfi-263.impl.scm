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

(define (add-slot! type message-alist slot-alist parent-list slot . args)
  (let* ((setter? (> 1 (length args)))
         (setter-name (and setter? (car args)))
         (value (if setter? (cdr args) (car args)))
         (getter (if (eq? type 'method)
                     value
                     (lambda (self resend) ; wrap the value in value slots 
                       value)))       ; for more streamlined access
         (setter (lambda (new-value)
                   (when (eq? type 'parent)
                     (set! parent-list
                       (lset-adjoin eq?
                                    (lset-xor eq? parent-list (list value))
                                    new-value)))
                   (set! value new-value)))
         (msgs-with-getter (alist-set! message-alist slot getter))
         (msgs-with-setter (alist-set! msgs-with-getter setter-name setter)))
    (values                                             ; returning...
     (if setter? msgs-with-setter msgs-with-getter)     ; message-alist
     (alist-set! slot-alist slot                        ; slot-alist
                 (list slot setter-name type value))
     (if (eq? type 'parent)                             ; parent-list
         (lset-adjoin eq? parent-list value) parent-list))))

(define (direct-slot-lookup self name message-alist)
  (let ((slot-alist ((self 'mirror) 'immediate-message-alist)))
    (cond ((assq name slot-alist) => cdr)
          (else #f))))

(define (recursive-method-lookup self name message-alist)
  (cond
   ((assq name message-alist)
    => (lambda (messages-entry)
         (values self (cdr messages-entry) #t)))
   (else
    (let ((mirror (self 'mirror)))
      (let loop ((parents (mirror 'immediate-ancestor-list))
                 (handler-count 0)
                 (receiver #f)
                 (handler #f)
                 (found #f))
        (if (null? parents)
            (if handler
                (if (= 1 handler-count)
                    (values receiver handler found)
                    (values #f 'ambiguous-message-send #f))
                (values #f 'message-not-understood #f))
            (let-values (((new-receiver new-handler new-found)
                          (recursive-method-lookup (car parents) name)))
              (loop (cdr parents)
                    (if new-found (add1 handler-count) handler-count)
                    (if new-found new-receiver receiver)
                    (if new-found new-handler handler)
                    (or new-found found)))))))))

(define (recursive-ancestor-collector self)
  (let ((parents ((self 'mirror) 'immediate-ancestor-list)))
    (if (null? parents)
        (list self)
        (apply lset-union
               eq?
               (list self)
               parents
               (map recursive-ancestor-collector parents)))))

(define (make-resender-template self parent-list)
  (lambda (#!optional args)
    #f))

(define (mirrorbox instance message-alist slot-alist parent-list)
  (letrec
      ((mirror
        (lambda (message . args)
          (case message
            ((immediate-ancestor-list) parent-list)
            ((full-ancestor-list) (recursive-ancestor-collector instance))
            ((immediate-message-alist) message-alist)
            ((immediate-slot-list) (map cdr slot-alist))
            (else
             (error "Message not understood:" message))))))
    mirror))

(define *the-root-object*
  (letrec
      ((object
        (lambda (message-alist slot-alist parent-list)
          (letrec
              ((obj-handler
                (lambda (command . args)
                  (let-values (((receiver method found?)
                                (recursive-method-lookup
                                 obj-handler command message-alist)))
                    (apply method obj-handler #f args))))
               (add-*-slot!
                (lambda (type)
                  (lambda (self resend name . args)
                    (let-values
                        (((new-msgs new-slots new-parents)
                          (apply add-slot! type
                                 message-alist slot-alist parent-list name args)))
                      (set! message-alist new-msgs)
                      (set! slot-alist new-slots)
                      (set! parent-list new-parents))))))
            (set! message-alist
              `((mirror .
                        ,(lambda (self resend)
                           (mirrorbox self message-alist slot-alist parent-list)))
                (clone .
                       ,(lambda (self resend)
                          (let-values
                              (((message-alist slot-alist parent-list)
                                (add-slot! 'parent '() '() '() 'parent self)))
                            (object message-alist slot-alist parent-list))))
                (delete-slot! .
                              ,(lambda (self resend name)
                                 (delete-slot! message-alist slot-alist parent-list name)))
                (add-value-slot! . ,(add-*-slot! 'value))
                (add-method-slot! . ,(add-*-slot! 'method))
                (add-parent-slot! . ,(add-*-slot! 'parent))))
            obj-handler))))
    (object '() '() '())))

;;;; Method running

;; (define (wrap-handler handler handler-name self args handler-messages)
;;   (lambda ()
;;     (apply handler
;;            self
;;            (make-resender handler handler-name self handler-messages)
;;            args)))

;; (define (make-resender handler-name handler-messages)
;;   (lambda (self . args)
;;     (messages-handle handler-messages self handler-name args)))

;; (define (run-with-error-checking handler self name args)
;;   (case handler
;;     ((message-not-understood)
;;      (if (eq? name 'message-not-understood)
;;          (error "Message MESSAGE-NOT-UNDERSTOOD not understood"
;;                 self args)
;;          (self 'message-not-understood name args)))
;;     ((ambiguous-message-send)
;;      (if (eq? name 'ambiguous-message-send)
;;          (error "Message AMBIGUOUS-MESSAGE-SEND is ambiguous"
;;                 self args)
;;          (self 'ambiguous-message-send name args)))
;;     (else (handler))))

;;;; Core object system
