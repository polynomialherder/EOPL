#lang racket

;; SchemeList ::= SchemeVal | ( SchemeVal . SchemeList ) 

;; duple : Int x Int -> SchemeList
;; usage: (duple n x) = A list containing n copies 
;;                      of x. 

(define duple
  (lambda (n x)
    (if (zero? n) '()
        (cons x (duple (- n 1) x)))))

;; tests 
; (duple 2 3)
; (duple 4 '(ha ha))
; (duple 0 '(blah))
; (duple 1 '(blah))
; (duple 3 0)

;; down : SchemeList -> SchemeList
;; usage: (down lst) = A list with elements of lst wrapped 
;;                     in parentheses. 

(define down 
  (lambda (lst)
    (if (null? lst) '()
        (cons (list (car lst)) (down (cdr lst))))))


(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))
    
