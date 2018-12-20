#lang racket/base

(require racket/function
         racket/contract
         racket/match
         racket/format
         racket/list
         pollen/decode
         txexpr
         threading
         "tag-utils.rkt"
         "contracts.rkt"
         "mark.rkt")

(provide decoder
         decoder/paragraph
         decoder/smart-typo
         decoder/mark

         exclusion-mark-attr-all
         exclusion-mark-attr-smart-typo
         exclusion-mark-attr-paragraph)

(define exclusion-mark-attr-all '(decode "exclude-all"))
(define exclusion-mark-attr-mark '(decode "exclude-mark"))
(define exclusion-mark-attr-smart-typo '(decode "exclude-smart-typo"))
(define exclusion-mark-attr-paragraph '(decode "exclude-paragraph"))

(define/contract (decoder tx) (content? . -> . content?)
  (~> tx
      decoder/paragraph
      decoder/smart-typo
      decoder/mark))

(define (decoder/paragraph tx)
  (decode tx
          #:txexpr-elements-proc decode-paragraphs
          #:exclude-tags '(style script pre code)
          #:exclude-attrs (list exclusion-mark-attr-all
                                exclusion-mark-attr-paragraph)))

(define (decoder/smart-typo tx)
  (decode tx
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script pre code)
          #:exclude-attrs (list exclusion-mark-attr-all
                                exclusion-mark-attr-smart-typo)))

(define (decoder/mark tx)
  (decode tx
          #:inline-txexpr-proc decode-mark
          #:exclude-tags '(style script)
          #:exclude-attrs (list exclusion-mark-attr-all
                                exclusion-mark-attr-mark)))
