#lang racket/base

(provide blog-header)
(require racket/contract
         "../utils/utils.rkt"
         "../rkt/contracts.rkt"
         "../config.rkt")

(define/contract (blog-header) (-> content?)
  `(a ([href ,(build-path-string path-prefix "feed.xml")]
       [class "btn btn-rss"])
      (i ([class "fas fa-rss"]))
      " Subscribe"))
