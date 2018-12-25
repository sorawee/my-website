#lang racket/base

(provide extract-metas current-title title->string)
(require racket/contract
         pollen/core
         "../config.rkt")

(define/contract (extract-metas key) (symbol? . -> . any/c)
  (select-from-metas key (current-metas)))

(define/contract (title->string title) ((or/c string? list?) . -> . string?)
  (cond
    [(string? title) title]
    [else (cdr (or (assoc (string->symbol lang) title)
                   (assoc 'en title)))]))

;; TODO: after we transition to all dicts, remove the string case.
(define (current-title) (title->string (extract-metas 'title)))
