#lang racket #| Head-Tail / First-Rest Patterns and Recursion. |#

#| A "." in quoted list expressions and `match` list patterns means
    the next part after it is the rest.
 
 Main traditional terminologies in languages with pattern matching:
    • first and rest
    • head and tail |#

#;(head . tail)

'(1 . (2 3 4))
`(,(random 324) . ,(map random '(324 324 324 324)))

(match (map random '(2 324 324 324))
  [`(0 . ,tail) tail]
  [`(,head . ,tail) (displayln head)
                    (displayln tail)])

; Let's make `map` of unary f onto list l.

(define (map-1 f l)
  (match l
    ['() '()]
    [`(,head . ,tail)
     `(,(f head) . ,(map-1 f tail))]))

(define (append-2 L1 L2)
  (match L1
    ['() L2]
    [`(,head . ,tail)
     `(,head . ,(append-2 tail L2))]))

(require test-engine/racket-tests)

(check-expect (append-2 '(3 2 4) '(5 6 7))
              `(3 . ,(append-2 '(2 4) '(5 6 7))))


#| Native list is more efficient to add to front than back.
 Let's use that as motivation to review accumulating recursion,
 to get a linear-time `reverse`. 

 You might remember accumulating recursion from CSC148 as
  "making a helper with an extra parameter (the accumulator). |#

; The trace: does the work on the way "in/down" the recursion:
;  pre-order.
'() '(3 2 4 6 7)
'(3)  '(2 4 6 7)
'(2 3)  '(4 6 7)
'(4 2 3)  '(6 7)
'(6 4 2 3)  '(7)
'(7 6 4 2 3) '()

(define (our-reverse L)
  (define (rev reversed-so-far L-to-do)
    (match L-to-do
      [`(,head . ,tail)
       (rev `(,head . ,reversed-so-far) tail)]
      ['() reversed-so-far]))
  (rev '() L))

#| Recursion subsumes Iteration. |#

(require racket/block)
(define-syntax-rule (until <condition>
                      <body>
                      ...)
  (block (define (loop)
           (if <condition>
               (void)
               (block <body>
                      ...
                      (loop))))
         (loop)))

(define L '(3 2 4))

(define reversed-so-far '())
(define L-to-do L)
(until (empty? L-to-do)
  (match L-to-do
    [`(,head . ,tail)
     (set! reversed-so-far `(,head . ,reversed-so-far))
     (set! L-to-do tail)]))

(check-expect (our-reverse '(3 2 4))
              reversed-so-far)

(test)

