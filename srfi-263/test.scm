(include "srfi-263.scm")
(import (srfi 263))

(define testmethod
  (lambda (self resend) 'success))

;;; Basic Functionality

(assert (null? ((*the-root-object* 'mirror) 'immediate-ancestor-list)))
(assert (= 8 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))

(let ((class (*the-root-object* 'clone)))
  (assert (eq? *the-root-object* (car ((class 'mirror) 'immediate-ancestor-list))))

  (class 'add-method-slot! 'testmethod testmethod)
  (assert (eq? 'success (class 'testmethod)))

  (class 'add-value-slot! 'val 'set-val! 10)
  (assert (eq? 10 (class 'val)))
  (class 'set-val! 20)
  (assert (eq? 20 (class 'val)))
  (assert (= 4 (length ((class 'mirror) 'immediate-message-alist))))
  (class 'add-value-slot! 'val 40)
  (assert (eq? 40 (class 'val)))
  (assert (= 3 (length ((class 'mirror) 'immediate-message-alist))))
  )

;;; Inheritance

(let* ((firstlevel (*the-root-object* 'clone))
       (secondlevel (firstlevel 'clone)))
  (firstlevel 'add-method-slot! 'testmethod testmethod)
  (assert (eq? 'success (secondlevel 'testmethod)))
  (firstlevel 'add-value-slot! 'val 'set-val! 10)
  (assert (eq? 10 (secondlevel 'val)))
  (secondlevel 'set-val! 20)
  (assert (eq? 10 (firstlevel 'val)))
  (assert (eq? 20 (secondlevel 'val)))
  (firstlevel 'add-value-slot! 'val #f 30)
  (assert (eq? 30 (firstlevel 'val)))
  (assert (eq? 20 (secondlevel 'val)))

  (assert (= 1 (length ((firstlevel 'mirror) 'full-ancestor-list))))
  (assert (= 2 (length ((secondlevel 'mirror) 'full-ancestor-list)))))

;; ;;; Multiple Inheritance

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

;;; Syntax
(include "srfi-263-syntax.impl.scm")

(define-object testobject (*the-root-object*)
  (testmethod (self resend) 'success)
  (val 10)
  (testval set-testval! 50))

(assert (eq? 'success (testobject 'testmethod)))
(assert (eq? 10 (testobject 'val)))
(assert (eq? 50 (testobject 'testval)))
(testobject 'set-testval! 20)
(assert (eq? 20 (testobject 'testval)))

(define-method (testobject methodslot self resend a b)
  (+ a b))
(assert (eq? 50 (testobject 'methodslot 20 30)))
