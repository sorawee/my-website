#lang racket/base

(provide atime)
(require racket/function
         (for-syntax racket/base))

(define map-time (make-hash))

(define-syntax (atime stx)
  (syntax-case stx ()
    [(_ label whatever ...)
     #'(begin
         (let-values ([(result cpu real gc) (time-apply (thunk whatever ...) '())]
                      [(labelv) label])
           (hash-update! map-time labelv (λ (x) (+ x real)) 0)
           (printf "Running ~a\n" labelv)
           (printf "spending time: ~a, accum: ~a\n" real (hash-ref map-time labelv))
           (apply values result)))]))
