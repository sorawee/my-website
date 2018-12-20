#lang racket/base

(provide either? left right)

(struct left (v) #:transparent)
(struct right (v) #:transparent)
(define (either? v)
  (or (left? v) (right? v)))
