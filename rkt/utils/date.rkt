#lang racket/base

(provide list->date)

(require racket/date
         racket/contract)

(define/contract (list->date lst)
  ((or/c #f (list/c number? number? number?)) . -> . (or/c #f date*?))

  (and lst (seconds->date (apply find-seconds (append (list 0 0 0) (reverse lst))))))
