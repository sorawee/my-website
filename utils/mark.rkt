#lang racket/base

(provide mark clear-mark left-marker right-marker mark-now)

(require racket/match
         racket/format)

(define left-marker "⟦")
(define right-marker "⟧")

(define (mark class-name . xs)
  (append (list (~a left-marker class-name " "))
          xs
          (list (~a " " right-marker))))

(define (mark-now class-name . xs)
  `(span ([class ,(~a "highlight-" class-name)])
         ,@xs))

(define (clear-mark x)
  (match x
    [(list _ c ... _) c]
    [_ (list x)]))
