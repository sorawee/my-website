#lang racket/base

(provide normalize-fragment ! !!)
(require racket/match
         racket/list
         racket/contract
         txexpr
         "contracts.rkt")

;; normalize fragment elements
(define/contract (normalize-fragment xs) (list? . -> . list?)
  (append-map
   (match-lambda
     [(txexpr '@ _ elems) (normalize-fragment elems)]
     [(txexpr t attrs elems) (list (txexpr t attrs (normalize-fragment elems)))]
                [x (list x)]) xs))

(module+ test
  (require rackunit)
  (check-equal?
   (normalize-fragment `((@ "a" (@ "b") "c" (p (@ "d"))) "e" (@ "g")))
   `("a" "b" "c" (p "d") "e" "g")))

;; create a fragment element from a list of elements
(define/contract (! lst) (list? . -> . content?) `(@ ,@lst))
(define/contract (!! . xs) (content? ... . -> . content?) (! xs))
