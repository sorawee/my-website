#lang racket/base

(provide copy-button)
(require racket/format)

(define (copy-button id refid)
  `(div ([class "copy-button"])
        (button ([type "button"]
                 [class "btn btn-outline-secondary btn-sm"]
                 [data-clipboard-target ,(~a "#" id "-" refid)])
                (i ([class "far fa-copy"])))))
