#lang racket/base

(provide highlight highlight-match)
(require racket/match
         racket/function
         racket/format
         racket/string
         racket/contract
         racket/list
         pollen/decode
         txexpr
         (rename-in pollen/unstable/pygments [highlight $highlight])
         "utils/debugging.rkt"
         "utils/cache.rkt"
         "decoders.rkt"
         "tag-utils.rkt"
         "clipboard.rkt"
         "mark.rkt"
         "doc-uri.rkt")

(define highlight/fast (do-cache $highlight #:file "highlight.rktd"))

(define highlight/copyable
  (let ([refid 0])
    (make-keyword-procedure
     (λ (kws kw-args . args)
       (define out (keyword-apply highlight/fast kws kw-args args))
       (set! refid (add1 refid))
       (highlight-match
        (match-lambda
          [`(pre ,args ...) `(pre ([id ,(~a "code-id-" refid)]) ,@args)])
        `(div ([class "highlight-container"])
              ,out
              ,(copy-button "code-id" refid)))))))

(define (parenthesize/racket code-original pre-elem)
  (define left-paren? (curry equal? '(span ((class "p")) "(")))
  (define right-paren? (curry equal? '(span ((class "p")) ")")))
  (define left-bracket? (curry equal? '(span ((class "p")) "[")))
  (define right-bracket? (curry equal? '(span ((class "p")) "]")))
  (define left-focus?
    (match-lambda [`(span ([class "n"]) ,s) (string-prefix? s left-marker)]
                  [_ #f]))
  (define right-focus?
    (match-lambda [`(span ([class "n"]) ,s) (string-prefix? s right-marker)]
                  [_ #f]))
  (define left-thing? (λ (x) (ormap (λ (fn?) (fn? x)) (list left-paren? left-bracket? left-focus?))))
  (define right-thing? (λ (x) (ormap (λ (fn?) (fn? x)) (list right-paren? right-bracket? right-focus?))))

  (match-define (txexpr 'pre attrs lst) pre-elem)

  ;; this function finds consecutive parentheses and split it to single
  ;; parenthesis. Note that `p` is the Pygments class for parentheses
  (define/contract (normalize lst) ((listof xexpr?) . -> . list?)
    (match lst
      [(list `(span ((class "p")) ,str) rst ...)
       (append (map (λ (x) `(span ((class "p")) ,(string x))) (string->list str))
               (normalize rst))]
      [(list `(span ((class ,(and class (or "k" "nb")))) ,val) rst ...)
       (define uri (doc-uri (string->symbol val)))
       (cons `(span ((class ,class)) ,(if uri `(a ((href ,uri)) ,val) val))
             (normalize rst))]
      [(list fst rst ...) (cons fst (normalize rst))]
      [_ lst]))

  (define-values (parsed-flipped)
    (for/fold ([stack '()]) ([e (normalize lst)])
      (match e
        [(? right-thing? rp)
         (define (preprocess stack) (if (right-focus? rp) (rest stack) stack))
         (define-values (grouped new-stack)
           (splitf-at (preprocess stack) (negate left-thing?)))
         (values
          (match new-stack
            [(list (? left-thing? lp) _ ...)
             ;; we want the paren matching to be of right types
             ;; it's not OK to match ( with ]
             (match (list lp rp)
               [(list (? left-paren?) (? right-paren?)) #f]
               [(list (? left-bracket?) (? right-bracket?)) #f]
               [(list (? left-focus?) (? right-focus?)) #f]
               [_ (error 'mismatched-type-paren "in ~a" code-original)])
             (define reversed-group (reverse grouped))
             (match lp
               [(? left-focus? lp)
                (match-define `(span ([class "n"]) ,s) lp)
                (cons
                 `(mark ([class ,(~a "highlight-" (substring s 1))])
                        ;; also get rid of space
                        ,@(rest reversed-group))
                 (rest new-stack))]
               [else
                (cons `(span ([class "paren"]) ,lp ,@reversed-group ,e)
                      (rest new-stack))])]
            [_ (cons e stack)]))] ; if too many right parentheses
        [_ (values (cons e stack))])))

  (txexpr 'pre (cons (list 'class "racket") attrs) (reverse parsed-flipped)))

(define (highlight #:lineno? [lineno? #t] lang . code-original)
  (println "highlight is called!!!")
  (define code
    (apply
     highlight/copyable
     (cons lang (decode (! code-original)
                        #:inline-txexpr-proc (compose1 flatten-mark
                                                       decode-fragment)))
     #:python-executable "python3"
     #:line-numbers? lineno?))

  (match lang
    ['racket (highlight-match (curry parenthesize/racket code-original) code)]
    [_ code]))


(define (highlight-match f blob #:class [class ""])
  (match blob
    [`(div ,attrs
       (div ((class "highlight"))
            (table ((class "sourcetable"))
                   (tbody
                    (tr ,linenos ...
                        (td ((class "code"))
                            (div ((class "source")) ,pre) "\n")))) "\n")
       ,copy-btn)
     `(div ,attrs
       (div ((class "highlight"))
            (table ((class "sourcetable"))
                   (tbody
                    (tr ,@linenos
                        (td ((class "code"))
                            (div ((class "source")) ,(f pre)) "\n")))) "\n")
       ,copy-btn)]))
