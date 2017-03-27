#lang racket

(provide test-func)

;; test-func : Func x List-of-FuncArgs x List-of-Expecteds -> List-of-Bool

;; usage:  (test-func func func-tests expected) = lob, where lob is 
;;         a list of booleans indicating whether the test 
;;         in the corresponding position of func-tests passed or 
;;         failed (i.e. matched the value in the corresponding position of
;;         expected). for example, if all tests pass, all elements
;;         of lob will be #t. likewise, if all fail, all will be
;;         #f. If func-tests is null, then the empty list is returned.         
    
(define test-func
  (lambda (func func-tests expected)
    (if (null? func-tests) '()
;; TODO -- currently assume func returns no errors.
;;         Refactor to return #f if func errors. 
        (if (equal? (apply func (car func-tests)) (car expected))
          (cons #t (test-func func (cdr func-tests) (cdr expected)))
          (cons #f (test-func func (cdr func-tests) (cdr expected)))))))
