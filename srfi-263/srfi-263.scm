(define-library (srfi 263)
  (export *the-root-object*
          slot?
          slot-getter
          slot-setter
          slot-type)
  (include "srfi-263.impl.scm"))
