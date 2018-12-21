#lang racket/base

(require racket/match
         "tag-utils.rkt"
         "../config.rkt"

         (for-syntax racket/base))

(define-syntax (make-languages stx)
  (syntax-case stx ()
    [(_ languages ...)
     #'(begin
         (provide languages ...)
         (define (languages . xs)
           (match (string->symbol lang)
             [(or 'all 'languages) (! xs)]
             [_ '(@)])) ...)]))

(make-languages thai eng)
