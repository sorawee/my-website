#lang racket/base

(provide blog-header)
(require racket/contract
         "../rkt/utils/path.rkt"
         "../rkt/contracts.rkt"
         "../config.rkt")

(define/contract (blog-header) (-> content?)
  `(a ([href ,(build-path-string path-prefix-lang "feed.xml")]
       [class "btn btn-rss"])
      (i ([class "fas fa-rss"]))
      " Subscribe"))
