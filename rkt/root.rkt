#lang racket/base

(provide define/root)
(require "utils/cache.rkt"
         (for-syntax racket/base))

(define-syntax (define/root stx)
  (syntax-case stx ()
    [(_ bindings body ...)
     #`(define #,(datum->syntax stx 'root) (λ bindings
                                             (cleanup)
                                             body ...))]))
