#lang racket

(require "../test.rkt")

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
        (if (eqv? (car los) s) 
          (cdr los)
          (cons (car los) (remove-first s (cdr los)))))))

;; tests
; (remove-first 'a '(a b c))
; (remove-first 'b '(e f g))
; (remove-first 'a4 '(c1 a4 c1 a4))
; (remove-first 'x '())

;; Ex. 1.8 [*]: In the definition of remove-first, if the last line 
;; were replaced by (remove-first s (cdr los)), what function would 
;; the resulting procedure compute? Give the contract, including the 
;; usage statement, for the revised procedure. 

;; Ans Ex. 1.8: Let lst be a List-of-Symbol. If lst is empty, then  
;; (remove-first s lst) will return '(). Suppose lst is non-empty, and 
;; that the first element of lst is equal to the target symbol s. 
;; Then (remove-first s lst) will return the cdr of lst (which may 
;; be empty). Suppose lst is non-empty, and that its first element
;; is not equal to s. Then we will call (remove-first s) on the cdr  
;; of lst. 
;;
;; Continually discarding the head of lst yields two possibilities. 
;; The first possibility is that the element s is encountered. 
;; In this case, the cdr of lst is returned without the first occurrence
;; of s. 
;;
;; The second possibility is that the element s is never encountered, or 
;; that it is encountered in the car of lst. In this case, the entire cdr of 
;; lst is returned. 
;;
;; More succinctly, we can write the contract for the updated function
;; as follows: 

;; remove-first-updated : Symbol x List-of-Symbols -> List-of-Symbols
;; usage: (remove-first-updated s los) returns the cdr of los with the same 
;;        elements arranged in the same order as cdr los, without the first 
;;        occurrence of s.

(define remove-first-updated
  (lambda (s los)
    (if (null? los) '()
        (if (eqv? (car los) s) 
          (cdr los)
          (remove-first s (cdr los))))))

;; tests 
;; (remove-first-updated 'a '(a b c d e f g))   ;; Expected: '(b c d e f g)
;; (remove-first-updated 'a '(b c d e a f g h)) ;; Expected: '(c d e a f g h) 
;; (remove-first-updated 'a '(b d e f g h i))   ;; Expected: '(d e f g h i)
;; (remove-first-updated 'a '(a b c d a b c d)) ;; Expected: '(b c d a b c d)
;; (remove-first-updated 'a '(d b c d a b c d)) ;; Expected: '(b c d b c d)
;; (remove-first-updated 'a '())                ;; Expected: '()
;; (remove-first-updated 'a '(d b c d b c d a)) ;; Expected: '(b c d b c d)

;; Ex. 1.9 [**] Define remove, which is like remove-first, except that
;; it removes all occurrences of a given symbol from a list of symbols,
;; not just the first.

;; remove : Symbol x List-of-Symbols -> List-of-Symbols
;; usage: (remove-first-updated s los) = los with all instances of 
;;        s removed. 

(define remove
  (lambda (s los)
    (if (null? los) '()
        (if (eqv? (car los) s) 
          (remove s (cdr los))
          (cons (car los) (remove s (cdr los)))))))

; (remove 'a '(a b c d e f g))   ;; Expected: '(b c d e f g)
; (remove 'a '(b c d e a f g h)) ;; Expected: '(b c d e f g h) 
; (remove 'a '(b d e f g h i))   ;; Expected: '(b d e f g h i)
; (remove 'a '(a b c d a b c d)) ;; Expected: '(b c d b c d)
; (remove 'a '(d b c d a b c d)) ;; Expected: '(d b c d b c d)
; (remove 'a '())                ;; Expected: '()
; (remove 'a '(d b c d b c d a)) ;; Expected: '(d b c d b c d)

(define remove-tests (list (list 'a '(a b c d e f g))    ;; Expected: '(b c d e f g)
                           (list 'a '(b c d e a f g h))  ;; Expected: '(b c d e f g h) 
                           (list 'a '(b d e f g h i))    ;; Expected: '(b d e f g h i)
                           (list 'a '(a b c d a b c d))  ;; Expected: '(b c d b c d)
                           (list 'a '(d b c d a b c d))  ;; Expected: '(d b c d b c d)
                           (list 'a '())                 ;; Expected: '()
                           (list 'a '(d b c d b c d a)))) ;; Expected: '(d b c d b c d)


(define remove-expected (list (list 'b 'c 'd 'e 'f 'g)
                              (list 'b 'c 'd 'e 'f 'g 'h)
                              (list 'b 'd 'e 'f 'g 'h 'i)
                              (list 'b 'c 'd 'b 'c 'd)
                              (list 'd 'b 'c 'd 'b 'c 'd)
                              (list)
                              (list 'd 'b 'c 'd 'b 'c 'd)))
