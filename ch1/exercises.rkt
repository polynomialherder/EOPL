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

;; SchemeList-of-pairs ::= '() | ( Pair . SchemeList-of-Pairs ) 
;;               Pair  ::= ( SchemeVal . SchemeVal ) 

;; invert : SchemeList-of-pairs -> SchemeList-of-pairs
;; usage: (invert lst) = a SchemeList-of-pairs, with the head
;;                       and cdr of each pair interposed. 

(define invert
  (lambda (lst)
    (if (null? lst) '()
        (cons (list (car (cdr (car lst))) (car (car lst))) 
              (invert (cdr lst))))))

; tests
; (invert '((1 a) (1 b) (1 c) (1 d)))
; (invert '())
; (invert '((1 2)))
; (invert '((1 2) (3 4) (5 6)))


;; down : SchemeList -> SchemeList
;; usage: (down lst) = A list with elements of lst wrapped 
;;                     in parentheses. 

(define down 
  (lambda (lst)
    (if (null? lst) '()
        (cons (list (car lst)) (down (cdr lst))))))

; tests
; (down '(1 2 3))
; (down '((a) (fine) (idea)))
; (down '(a (more (complicated)) object))
    
;; swapper : SchemeVal x SchemeVal x SchemeList -> SchemeList
;; usage: (swapper s1 s2 slist) = a new SchemeList with any occurrences
;;                                of s2 are replaced with s1. 

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist) '()
        (if (equal? (car slist) s2) 
            (cons s1 (swapper s1 s2 (cdr slist)))
            (cons (car slist) (swapper s1 s2 (cdr slist)))))))

; tests
; (swapper 'a 'd '(a b c d))
; (swapper 'a 'd '(a a a a d d d d ))
; (swapper 'a 'b '())
; (swapper '(1 2) '(a a) '((1 2) (1 2) (a a)))
; (swapper 'a 'a '(a b c d e a))
; (swapper '() 'a '(a b c d a a a))

;; list-set : SchemeList x Int x SchemeVal -> SchemeList 
;; usage: (list-set lst n x) = A scheme list where the element at
;;                             index n (assuming zero based indexing)
;;                             is replaced with the Scheme value x. 

(define list-set
  (lambda (lst n x)
    (if (null? lst) '()
        (if (zero? n) 
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

; tests
; (list-set '(a b c d) 2 '(1 2))
; (list-set '(a b c d) 3 '(1 5 10))
; (list-set '() 2 'a)

;; count-occurrences : SchemeVal x SchemeList -> Int
;; (count-occurrences s slist) = The number of occurrences of s
;;                               in slist.

(define count-occurrences
  (lambda (s slist)
    (if (null? slist) 0
        (if (equal? (car slist) s)
            (+ 1 (count-occurrences s (cdr slist)))
            (count-occurrences s (cdr slist))))))

; tests
; (count-occurrences 'a '(a b c d))
; (count-occurrences 'b '(b b b b))
; (count-occurrences 'b '(b))
; (count-occurrences 'c '())
; (count-occurrences '(1 2) '((1 2) (1 2) (1 2) (3 4) (5 6) (1 2)))

;; product : List-of-Symbols x List-of-Symbols -> List-of-2Lists
;; usage: (cartesian-product los1 los2) = A list of 2-lists that represent 
;;                                        the Cartesian product of los1 and
;;                                        los2. The 2-lists may appear in 
;;                                        any order.

;; List-of-2Lists ::= '() | ( 2List . List-of-2Lists ) 
;;         2List  ::= ( SchemeVal . SchemeVal ) 

;; Recall that the Cartesian product of two sets A and B 
;; is the set A x B = { (a, b) | a in A, b in B }. 
;; Note if A or B is empty, then the Cartesian product
;; is the empty set.

(define cartesian-product
  (lambda (los1 los2)
    (if (null? los1) '()
        (append (map (lambda (fix-lst-elem) (cons (car los1) fix-lst-elem)) los2)
                (cartesian-product (cdr los1) los2)))))


; tests
; (cartesian-product '(1 2) '(a b c d e f g)) 
; (cartesian-product '(a b c d e f g) '(1 2))
; (cartesian-product '() '())
; (cartesian-product '() '(a b c))
; (cartesian-product '(a b c) '())

