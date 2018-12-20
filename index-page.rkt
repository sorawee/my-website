#lang racket/base

(provide publication-list
         publication)
(require racket/list
         racket/contract
         "rkt/contracts.rkt"
         "rkt/tags.rkt")

(define (publication-list . xs) `(ul ([class "publication-list"]) ,@xs))

(define/contract (publication title paper-link authors conference)
  (string? string? list? string? . -> . content?)

  (item (div (link paper-link title))
        (apply div (add-between (map (λ (x) (if (eq? x 'me)
                                                (strong "Sorawee Porncharoenwase")
                                                x))
                                     authors) ", "))
        (div `(i ,conference))))
