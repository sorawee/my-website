#lang racket/base

(provide string-replace/all)
(require racket/string
         racket/contract)

;; perform a series of string-replace on str according to xs from left to right
(define/contract (string-replace/all xs str) (list? string? . -> . string?)
  (foldl (λ (item prev) (string-replace prev (car item) (cdr item))) str xs))

(module+ test
  (require rackunit)
  (check-equal?
   (string-replace/all '(("aa" . "b")
                         ("a" . "c"))
                       "aa")
   "b")
  (check-equal?
   (string-replace/all '(("a" . "b")
                         ("b" . "c"))
                       "ab")
   "cc"))
