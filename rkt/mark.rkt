#lang racket/base

(provide mark
         decode-mark flatten-mark remove-mark
         left-marker right-marker)

(require racket/match
         racket/format
         txexpr)

(define left-marker "⟦")
(define right-marker "⟧")

(define flatten-mark
  (match-lambda
    [(txexpr '@mark _ (list class-name xs ...))
     (append (list (~a left-marker class-name " ")) xs (list (~a " " right-marker)))]
    [tx tx]))

(define remove-mark
  (match-lambda
    [(txexpr '@mark _ (list _ xs ...)) xs]
    [tx tx]))

(define decode-mark
  (match-lambda
    [(txexpr '@mark _ (list class-name xs ...))
     `(mark ([class ,(~a "highlight-" class-name)]) ,@xs)]
    [tx tx]))

(define (mark . xs) `(@mark ,@xs))
