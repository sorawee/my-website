#lang pollen

@(define-meta special-title "My Blog")
@(define-meta type "index")

@(define _ (current-pagetree (get-pagetree "index.ptree")))
@(! (map (lambda (post) (make-post post #:content (get-summary (get-doc post)) #:see-more #t))
    (children 'blog)))
