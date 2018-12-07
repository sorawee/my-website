#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/date
         racket/path
         racket/contract
         pollen/setup)

(define/contract (directory-list-string path #:build? [build? #f])
  (path-string? #:build? boolean? . -> . (listof string?))
  (map path->string (directory-list path #:build? build?)))

(define/contract (build-path-string . xs) (path-string? ... . -> . string?)
  (path->string (apply build-path xs)))

(define/contract (list->date lst) ((or/c #f (list/c number? number? number?)) . -> . (or/c #f date*?))
  (and lst (seconds->date (apply find-seconds (append (list 0 0 0)
                                                      (reverse lst))))))

(define/contract (path-string->string p) (path-string? . -> . string?)
  (path->string (build-path p)))

(define/contract (rel-path p) (path-string? . -> . path-string?)
  (find-relative-path (current-project-root) p))
