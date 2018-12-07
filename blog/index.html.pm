#lang pollen

@(require "../utils/file.rkt")

@(define-meta title "My Blog")
@(define-meta type index)

@(blog-header)

@(! (map (curry make-post #:see-more? #t) (all-posts)))