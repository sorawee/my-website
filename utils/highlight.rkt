#lang racket/base

(provide highlight highlight-match)
(require racket/match
         racket/function
         racket/string
         racket/contract
         racket/list
         xml
         pollen/decode
         txexpr
         (rename-in pollen/unstable/pygments [highlight original-highlight])
         "../rkt/tag-utils.rkt"
         "../rkt/mark.rkt"
         "cache.rkt"
         "doc-uri.rkt")

(define (highlight/core lang code-original lineno?)
  (define out
    (apply
     original-highlight
     (cons lang (rest (decode (! code-original) #:inline-txexpr-proc flatten-mark)))
     #:python-executable "python3"
     #:line-numbers? lineno?))

  (match lang
    ['racket
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

     (define (parenthesize lst)
       ;; this function finds consecutive parentheses and split it to single
       ;; parenthesis. Note that `p` is the Pygments class for parentheses
       (define/contract (normalize lst) ((listof xexpr?) . -> . (listof xexpr?))
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
             (define-values (grouped new-stack) (splitf-at (preprocess stack) (negate left-thing?)))
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
                      `(span ([class ,(format "highlight-~a" (substring s 1))])
                             ;; also get rid of space
                             ,@(rest reversed-group))
                      (rest new-stack))]
                    [else
                     (cons `(span [[class "paren"]] ,lp ,@reversed-group ,e)
                           (rest new-stack))])]
                 [_ (cons e stack)]))] ; if too many right parentheses
            [_ (values (cons e stack))])))
      (reverse parsed-flipped))
      (highlight-match parenthesize out #:class "racket")]
    [_ (highlight-match values out #:class "other")]))

(define (highlight #:lineno? [lineno? #t] lang . code-original)
  (do-cache highlight/core lang code-original lineno?))

(define (highlight-match f blob #:class [class ""])
  (match blob
    [(or `(div ((class "highlight"))
               (table ((class "sourcetable"))
                      (tbody
                       (tr ,linenos ...
                           (td ((class "code"))
                               (div ((class "source")) ,pre) "\n")))) "\n")
         ;; makes linenos in the second case matches empty list
         `(div ((class "highlight"))
               (div ((class "source")) ,pre) "\n\n" ,linenos ...))
     (define class-name (string-append (second (or (assoc 'class (get-attrs pre))
                                                   (list "" ""))) " " class))
     (define things-in-pre (get-elements pre))
     `(div ((class "highlight"))
           (table ((class "sourcetable"))
                  (tbody
                   (tr ,@linenos
                       (td ((class "code"))
                           (div ((class "source"))
                                (pre ((class ,class-name))
                                     ,@(f things-in-pre))) "\n")))) "\n")]))
