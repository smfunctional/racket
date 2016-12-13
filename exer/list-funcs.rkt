#lang racket

; recursive definition of append
(define (append-r l1 l2)
  (cond
    [(null? l1) l2]
    [else (cons (car l1) (append-r (cdr l1) l2))]))

; Define append using fold
(define (append-t l1 l2)
  (foldr cons l2 l1))

(append-r '( 1 2 3) '(4 5 6))
(append-t '( 1 2 3 7) '(4 5 6))
