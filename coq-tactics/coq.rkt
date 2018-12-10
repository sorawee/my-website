#lang racket/base

(provide coq-tactic coq usage use-when tac coq-interactive)
(require racket/sequence
         racket/list
         racket/pretty
         racket/function
         racket/match
         racket/system
         racket/format
         racket/port
         racket/string
         threading
         "../utils/highlight.rkt"
         "../utils/cache.rkt"
         "../utils/mark.rkt")

(define current-tactic (make-parameter #f))

#;(define (resolve-goals xs prev)
  (match xs
    [(list) '()]
    [(list (== same-as-previous) rst ...) (cons prev (resolve-goals rst prev))]
    [(list fst rst ...) (cons fst (resolve-goals rst fst))]))

(define (analyze/stream xs acc-cmd)
  (match xs
    [(list) (list (reverse acc-cmd))]
    [(list fst rst ...)
     (cond
       [(string-prefix? fst "<prompt>")
        (define fst-truncated
          (regexp-replaces fst '([#px"<prompt>.*?</prompt>" ""])))
        (cons (reverse acc-cmd) (analyze/stream rst (list fst-truncated)))]
       [else (analyze/stream rst (cons fst acc-cmd))])]))

(define (analyze/error xs)
  (define-values (left right) (splitf-at xs (λ (lines) (not (string-prefix? (first lines) "Toplevel input,")))))
  (if (cons? right)
      (append left (list (first right)))
      left))

(define (analyze/message s)
  (define collected '())
  (define (collect _ matched)
    (set! collected (cons matched collected))
    "")
  (cons (regexp-replaces s (list (list #px"<infomsg>(.*?)</infomsg>\\s*" collect)))
        (string-join (reverse collected) "\n")))

(define (analyze/prev xs prev)
  (match xs
    [(list) '()]
    [(list (cons (pregexp #px"^\\s*$") info) rst ...)
     (cons (cons prev info) (analyze/prev rst prev))]
    [(list fst rst ...) (cons fst (analyze/prev rst (car fst)))]))

(define (analyze/combine pair) (string-append (car pair) "\n\n" (cdr pair)))

(define (coq-interactive . xs)
  (do-cache coq-interactive/private xs #:file "coq.rktd"))

(define (coq-interactive/private xs)
  (define code (apply string-append (apply append (map clear-mark xs))))
  (match-define (list in out _ err proc)
    (process (~a (find-executable-path "coqtop") " -emacs 2>&1")))
  (display code out)
  (close-output-port out)
  (proc 'wait)
  (define output '())
  (let loop ()
    (match (read-line in)
      [(? string? v) (set! output (cons v output))
                     (loop)]
      [_ (void)]))
  (close-input-port in)
  (close-input-port err)
  (proc 'kill)

  (set! output (rest (reverse output)))

  (apply coq-box
         (apply coq-block xs)
         (~> output
             (analyze/stream _ '())
             (drop-right _ 1) ;; the last one is always blank
             analyze/error
             (map (curryr string-join "\n") _)
             (map analyze/message _)
             (analyze/prev _ "")
             (map analyze/combine _)
             (map coq-block _))))

(define (coq-tactic . xs)
  (current-tactic xs)
  `(h3 ([class "coq-tactic"]) (code ,@xs)))

(define (coq . xs) `(code ,@xs))

(define refid 0)

(define (coq-box script . xs)
  (set! refid (add1 refid))
  (define items
    (for/list ([item (in-slice 1 xs)]
               ;; change 1 to 2 if we want to add msgbox
               [i (in-naturals)])
      `(@ (div ([class ,(format "coq-pane col-md-6 show-all show-~a" i)])
               ,(first item)))))
  `(@ (div ([class "coq-box row no-gutters"]
            [id ,(format "coq-box-~a" refid)])
           (div ([class "coq-script col-md-6"]) ,script)
           ,@items)
      (div ([class "row coq-buttons"])
           (div ([class "col text-center"]
                 [data-size ,(number->string (length xs))]
                 [data-refid ,(number->string refid)])
                (button ([class "prev-button mr-1"]) "Previous")
                (button ([class "next-button ml-1"]) "Next")))))

(define sep "TERM1NAT1NGSEPARAT0R")
(define sep-span `(span ([class "n"]) ,sep))

(define (transform-term-sep toks acc i)
  (define (make-next obj)
    `((span ([class ,(format "show-all show-~a" i)]) ,@(reverse obj))))

  (match toks
    [(list) (reverse acc)]
    [(list (and fst `(span ([class "o"])
                           ,(and sym (or "." "{" "}" (pregexp #px"[-+*]+")))))
           rst ...)
     (define cont?
       (cond
         [(string=? "." sym) #t]
         [(string=? "{" sym) #t]
         [(string=? "}" sym) #t]
         [else (match acc
                 [(list (pregexp #px"\n *") _ ...) #t]
                 [_ #f])]))
     (if cont?
         (transform-term-sep
          rst
          (make-next (cons fst acc))
          (add1 i))
         (transform-term-sep rst (cons fst acc) i))]
    [(list '(span ([class "err"]) "⟦")
           `(span ([class "n"]) ,class-name)
           " "
           (and inside (not '(span ([class "err"]) "⟧"))) ...
           " "
           '(span ([class "err"]) "⟧")
           rst ...)
     (define next (cons `(span ([class ,(format "highlight-~a" class-name)])
                               ,@inside)
                        acc))
     (cond
       [(equal? `(span ([class "o"]) ".") (last inside))
        (transform-term-sep rst (make-next next) (add1 i))]
       [else (transform-term-sep rst next i)])]
    [(list fst rst ...) (transform-term-sep rst (cons fst acc) i)]))

(define (coq-block . xs)
  (highlight-match
   (curryr transform-term-sep '() 1)
   (apply highlight "coq" xs)))

(define (usage . xs) `(div ([class "coq-usage"]) (b "Usage: ") ,@xs))

(define (use-when . xs) `(div ([class "coq-use-when"]) (b "Use it when: ") ,@xs))

(define (tac . xs) `(code ,@(current-tactic) ,@xs))
