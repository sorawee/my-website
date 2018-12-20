#lang racket/base

(provide extract-metas current-title)
(require racket/contract pollen/core)

(define/contract (extract-metas key) (symbol? . -> . any/c)
  (select-from-metas key (current-metas)))

(define (current-title) (extract-metas 'title))
