#lang racket

;; Exercise 1.10 [*]: We typically use "or" to mean "inclusive or". 
;;                    What other meanings can "or" have?

;; Ans. 1.10: We can also mean "exclusive or", which we can write XOR, 
;;            which is true when its arguments have different truth 
;;            values and false otherwise. 

;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; occurs-free? : Sym x LcExp -> Bool 
;; usage:  returns #t if the symbol var occurs free
;;         in exp, otherwise returns #f. 

(define occurs-free? 
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda) 
       (and 
         (not (eqv? var (car (cadr exp))))
         (occurs-free? var (caddr exp))))
      (else
        (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp)))))))
