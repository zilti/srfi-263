(include "srfi-263.impl.scm")

(define testmethod
  (lambda () 'success))

(assert (null? ((*the-root-object* 'mirror) 'immediate-ancestor-list)))

(assert (= 6 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))

(let ((cloneroot (*the-root-object* 'clone)))
  (cloneroot 'add-value-slot! 'myval 5)
  (assert (= 5 (cloneroot 'myval)))
  (assert (= 6 (length ((*the-root-object* 'mirror) 'immediate-message-alist))))
  (assert (= 7 (length ((cloneroot 'mirror) 'immediate-message-alist))))
  )

(let* ((cloneroot (*the-root-object* 'clone))
       (clonecloneroot (cloneroot 'clone)))
  (assert (eq? '()
               (lset-difference eq?
                                (list *the-root-object* cloneroot clonecloneroot)
                                ((clonecloneroot 'mirror) 'full-ancestor-list)))))
