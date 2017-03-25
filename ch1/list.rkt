#lang racket

;; List ::= () | (Scheme value . List)

;; list-length : List -> Int
;; usage: (list-length l) = the length of l

(define list-length
  (lambda (lst)
    (if (null? lst) 0
      (+ 1 (list-length (cdr lst))))))

;; nth-element : List x Int -> SchemeVal 
;; usage: (nth-element l n) = the value at the nth position of l

(define nth-element
  (lambda (lst n)
    (if (null? lst)
      (report-list-too-short n)
      (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (lst n)
    (error 'nth-element
      "List too short by ~s elements.~%" (+ n 1))))

;; Ex 1.6 [*]: If we reversed the order of the tests in nth-element, 
;; what would go wrong? 

;; Ans. Ex 1.6: If we reverse the order of the tests, then we test
;; whether n is 0 before we determine if lst is null. If n is 0 and 
;; lst is null, then we obtain an error because such a value is not
;; defined. 

;; Ex 1.7 [**]: The error message from nth-element is uninformative. 
;; Rewrite nth-element so that it produces a more informative 
;; error message, such as "(a b c) does not have 8 elements.

;; Ans. Ex 1.7: 

(define nth-element-improved
  (lambda (lst n)
 ; FIX ME: 
 ; Not ideal because list-length is called redundantly -- it need 
 ; only be called once to test the initial length of the list. 
    (if (<= (list-length lst) n)
      (report-ltsi-error lst n)
      (if (zero? n) (car lst)
           (nth-element-improved (cdr lst) (- n 1))))))

(define report-ltsi-error 
  (lambda (lst n) 
    (error 'nth-element-improved
       "List ~s does not have ~s elements" lst n))) 

;; List-of-Symbol ::= () | (Symbol . List-of-Symbol)
;; remove-first : Symbol x List-of-Symbols -> List-of-Symbols
;; usage: (remove-first s los) returns a list with the same 
;;        elements arranged in the same order as los, except
;;        that the first occurrence of the symbol s is removed. 

(define remove-first
  (lambda (s los)
    (if (null? los) '()
        (if (= (car los) s) 
          (remove-first s (cdr los))
          ( 
