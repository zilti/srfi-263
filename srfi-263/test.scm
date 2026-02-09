(include "srfi-263.impl.scm")

(define testmethod
  (lambda () 'success))

(assert (null? ((*the-root-object* 'mirror) 'immediate-ancestor-list)))
(assert (= 6 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))

(let ((cloneroot (*the-root-object* 'clone)))
  (cloneroot 'add-value-slot! 'myval 5)
  (assert (= 5 (cloneroot 'myval)))
  (assert (= 6 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))
  (assert (= 8 (length ((cloneroot 'mirror) 'immediate-message-alist))))
  )

(let* ((cloneroot (*the-root-object* 'clone))
       (clonecloneroot (cloneroot 'clone)))
  (assert (eq? '()
               (lset-difference eq?
                                (list *the-root-object* cloneroot clonecloneroot)
                                ((clonecloneroot 'mirror) 'full-ancestor-list)))))


;;; Multiple Inheritance

(let* ((adderclass (*the-root-object* 'clone))
       (squareclass (*the-root-object* 'clone))
       (mathclass (squareclass 'clone)))
  (adderclass 'add-method-slot! 'add1
              (lambda (self resend val)
                (add1 val)))
  (squareclass 'add-method-slot! 'square
               (lambda (self resend val)
                 (* val val)))
  (mathclass 'add-parent-slot! 'adder adderclass)
  (assert (= 10 (adderclass 'add1 9)))
  (assert (= 9 (squareclass 'square 3)))
  (assert (= 9 (mathclass 'add1 8)))

  (assert
   (call-with-current-continuation
    (lambda (cont)
      (with-exception-handler
          (lambda (exc) (cont #t))
        (lambda () (adderclass 'sub1 10)
           (cont #f))))
    ))

  (adderclass 'add-method-slot! 'reset (lambda (self resend x) 5))
  (squareclass 'add-method-slot! 'reset (lambda (self resend x) 5))

  (assert
   (call-with-current-continuation
    (lambda (cont)
      (with-exception-handler
          (lambda (exc) (cont #t))
        (lambda () (mathclass 'reset 1)
           (cont #f))))))
  )

;;; Value slots

;; (let ((firstlevel (*the-root-object* 'clone)))
;;   (firstlevel 'add-value-slot! 'number 'set-number! 0)
;;   (firstlevel 'add-method-slot! 'mod-value (lambda (self resend)
;;                                              (self 'set-number! 5)))
;;   (let ((secondlevel (firstlevel 'clone)))
;;     (secondlevel 'add-method-slot! 'mod-value (lambda (self resend)
;;                                                 (display "Calling resend...\n")
;;                                                 (resend #f)
;;                                                 (display "Called resend.\n")))
;;     (display "Testing resend\n")
;;     (secondlevel 'mod-value)))
