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

;; TODOS -- 
;;         (*) Current implementation assumes func returns no errors.
;;             Refactor to cons #f if func errors. 
;;         (*) Requiring the parameters func-tests and expected as two 
;;             separate lists is obviously problematic. 
;;             Refactor using a hash table so that the 
;;             signature of the input is Func x Hash-table.
;;         (*) Count the number of pass/fails, providing specific 
;;             information about the failed inputs. 
;;         (*) Allow an optional description of each test input.    
;;         (*) Pretty print test results. 
    
(define test-func
  (lambda (func func-tests expected)
    (if (null? func-tests) '()
        (if (equal? (apply func (car func-tests)) (car expected))
          (cons #t (test-func func (cdr func-tests) (cdr expected)))
          (cons #f (test-func func (cdr func-tests) (cdr expected)))))))
