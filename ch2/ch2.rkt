#lang racket 

;; Bignum representation -- 
;; Numbers are represented in base N for some large integer N. 
;; n ::= '() | (r . [q]) 
;; where n = qN + r, 0 <= r < N 

(define base 2)

(define zero (lambda () '()))

(define is-zero? (lambda (n) (null? n)))

(define successor
  (lambda (n)
    (cond ((is-zero? n) '(1))
          ((= (+ (car n) 1) base) (cons 0 (successor (cdr n))))
          (else (cons (+ 1 (car n)) (cdr n))))))

(define predecessor 
;; TO DO 
  (lambda (n)
    #f))

;; tests 
(is-zero? (zero))
(successor (zero))
(successor (successor (successor (successor (successor (successor (successor (successor (zero)))))))))
