#lang racket/base

(provide toplevel)
(require racket/string
         racket/function
         racket/contract
         racket/match
         pollen/setup
         pollen/template
         "utils/cache.rkt"
         "contracts.rkt")

(define tags
  '(;; from http://docs.racket-lang.org/pollen/Setup.html#%28def._%28%28lib._pollen%2Fsetup..rkt%29._default-block-tags%29%29
    address article aside
    blockquote body canvas
    dd div dl
    fieldset figcaption figure footer form
    h1 h2 h3 h4 h5 h6 header hgroup hr
    li
    main
    nav noscript
    ol output
    p pre
    section
    table tfoot
    ul
    video))

(define/contract (add-newlines s) (string? . -> . string?)
  (regexp-replace*
   (pregexp (format "(?=~a)|(?<=~a)"
                    (string-join (map (curry format "<~a>") tags) "|")
                    (string-join (map (curry format "</~a>") tags) "|")))

   s
   "\n"))

(define/contract (interp x) (content? . -> . content?)
  (match x
    [`(@@app ,f ,doc)
     (define transform (dynamic-require (build-path (current-project-root) f)
                                        'transform))
     (interp (transform doc))]
    [_ x]))

(define (cleanup* v)
  (cleanup)
  v)

(define toplevel (compose1 cleanup* add-newlines ->html interp))
