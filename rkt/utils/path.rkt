#lang racket/base

(provide directory-list-string build-path-string rel-path slug)

(require racket/path
         racket/contract
         racket/format
         threading
         pollen/setup)

(define/contract (directory-list-string path #:build? [build? #f])
  (path-string? #:build? boolean? . -> . (listof string?))
  (map path->string (directory-list path #:build? build?)))

(define/contract (build-path-string . xs) (path-string? ... . -> . string?)
  (path->string (apply build-path xs)))

(define/contract (rel-path p) (path-string? . -> . path-string?)
  (find-relative-path (current-project-root) p))


;; From https://github.com/greghendershott/frog/blob/master/frog/paths.rkt
;; Convert a string/symbol into a "slug" string

(define (slug s)
  (~>
   ;; First normalize string to Unicode composite form, so e.g. á will
   ;; be a single char for which char-alphabetic? is true. (In the
   ;; decomposed form á would be a plain a char followed by an accent
   ;; char, and the latter is not char-alphabetic? and would get
   ;; slugged to a hyphen.)
   (for/list ([c (in-string (string-normalize-nfc (~a s)))])
     (cond [(or (char-alphabetic? c)
                (char-numeric? c)) c]
           [else #\-]))
   list->string
   ;; Only one consecutive hyphen
   (regexp-replace* #px"-{2,}"  _ "-")
   ;; No trailing hyphen
   (regexp-replace  #px"-{1,}$" _ "")
   ;; Finally normalize to decomposed form. The rationale is that if
   ;; you use this result in a filename it will (hopefully) be
   ;; consistent across filesystems like Linux vs macOS.
   string-normalize-nfd))
