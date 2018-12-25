#lang racket/base

(provide ! !!)
(require racket/contract
         "contracts.rkt")

;; create a fragment element from a list of elements
(define/contract (! lst) (list? . -> . content?) `(@ ,@lst))
(define/contract (!! . xs) (content? ... . -> . content?) (! xs))
