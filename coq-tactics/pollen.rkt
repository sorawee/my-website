#lang racket/base

(provide (all-from-out "../pollen.rkt")
         root
         coq-tactic usage use-when tac coq-interactive relevant-tactics caveat
         additional-desc lookup-tac tactic lookup-uncat)

(require (except-in "../pollen.rkt" root)
         racket/sequence
         racket/list
         racket/pretty
         racket/function
         racket/match
         racket/system
         racket/format
         racket/port
         racket/string
         racket/set
         pollen/decode
         threading
         "../utils/highlight.rkt"
         "../utils/cache.rkt"
         "../utils/mark.rkt")

(module setup racket/base
  (provide (all-defined-out))
  (define allow-unbound-ids #f)
  (define command-char #\@))

(define (root . items)
  (decode `(decoded-root ,@items)
          #:txexpr-elements-proc decode-paragraphs
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:inline-txexpr-proc inline-txexpr-proc
          #:exclude-tags '(style script pre code)
          #:exclude-attrs (list exclusion-mark-attr)))

(define (linkify tname acc)
  (define (linkify/one s)
    (cond
      [(string? s)
       (add-between (string-split s tname #:trim? #f)
                    `(a ([href ,(~a "#tactic-" tname)]) ,tname))]
      [else (list s)]))
  (apply append (map linkify/one acc)))


(define collected-tactics '())
(define mentioned-tactics '())

(define (inline-txexpr-proc tx)
  (match tx
    [`(lookup-tac ,name ,index)
     (define result (assoc (cons name index) collected-tactics))
     (set! mentioned-tactics (cons (cons name index) mentioned-tactics))
     `(div ([class "inverted-tactic"])
           "If " ,@(cdr result) ".. use " (a ([href ,(~a "#tactic-" name)])
                                             (code ,(caar result))) ".")]
    [`(tactic ,xs ...)
     `(code ,@(foldl linkify xs (remove-duplicates
                                 (map caar collected-tactics))))]
    [`(lookup-uncat)
     `(@ ,@(map (λ (p) (inline-txexpr-proc `(lookup-tac ,(car p) ,(cdr p))))
                (set-subtract (map car collected-tactics) mentioned-tactics)))]
    [_ tx]))

(define current-tactic (make-parameter #f))

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
  (define-values (left right)
    (splitf-at xs (λ (lines) (not (string-prefix? (first lines) "Toplevel input,")))))
  (if (cons? right) (append left (list (first right))) left))

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
  (match-define (cons script boxes)
    (do-cache coq-interactive/private xs #:file "coq.rktd"))
  (apply coq-box script boxes))

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

  (cons (apply coq-block xs)
        (~> output
            (analyze/stream _ '())
            (drop-right _ 1) ;; the last one is always blank
            analyze/error
            (map (curryr string-join "\n") _)
            (map analyze/message _)
            (analyze/prev _ "")
            (map analyze/combine _)
            (map coq-block _))))

(define (coq-tactic s)
  (current-tactic s)
  `(h3 ([class "coq-tactic"]
        [id ,(~a "tactic-" s)]) ,(tactic s)))

(define (tactic . xs) `(tactic ,@xs))

(define refid 0)


(define (coq-box script . xs)
  (set! refid (add1 refid))
  (define items
    (for/list ([item (in-slice 1 xs)]
               ;; change 1 to 2 if we want to add msgbox
               [i (in-naturals)])
      `(@ (div ([class ,(format "coq-pane col-md-6 show-all show-~a" i)])
               ,(first item)))))
  `(@ (div ([class "coq-buttons row no-gutters"])
           (div ([class "col text-right"]
                 [data-size ,(number->string (length xs))]
                 [data-refid ,(number->string refid)])
                (button ([class "prev-button mr-1"]) "Previous")
                (button ([class "next-button ml-1"]) "Next")))
      (div ([class "coq-box row no-gutters"]
            [id ,(format "coq-box-~a" refid)])
           (div ([class "coq-script col-md-6"]) ,script)
           ,@items)))

(define (transform-term-sep toks acc i)
  ;; There are three possible end commands: dot, bullet, and curly bracket,
  ;; as we can see from Proof General's coq-indent.el
  ;;
  ;; (defconst coq-end-command-regexp
  ;;   (concat coq-period-end-command "\\|"
  ;;           coq-bullet-end-command "\\|"
  ;;           coq-curlybracket-end-command))
  ;;
  ;; We will parse code in a stupid way since the correct way seems very complex.
  ;; If there are cases that break, we will fix them later.

  (define (make-next obj)
    `((span ([class ,(format "show-all show-~a" i)]) ,@(reverse obj))))

  (match toks
    [(list) (reverse acc)]
    [(list (and fst `(span ([class "o"])
                           ,(and sym (or (pregexp #px"^.*\\.$") "{" "}" (pregexp #px"^[-+*]+$")))))
           rst ...)
     (define cont?
       (match sym
         [(pregexp #px"^.*\\.$") #t]
         ["{" #t]
         ["}" #t]
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


(define (use-when #:index [index "default"] . xs)
  (set! collected-tactics (cons (cons (cons (current-tactic) index) xs)
                                collected-tactics))
  `(div ([class "coq-use-when"]) (b "Use it when: ") ,@xs))

(define (relevant-tactics . xs)
  `(div ([class "coq-relevant"]) (b "Relevant tactics: ") (ul ,@xs)))

(define (additional-desc . xs)
  `(div ([class "coq-desc"]) (b "Additional description: ") ,@xs))

(define (caveat . xs)
  `(div ([class "coq-caveat"]) (b "Caveat: ") ,@xs))

(define (tac . xs) (apply tactic (current-tactic) xs))

(define (lookup-tac #:index [index "default"] name) `(lookup-tac ,name ,index))

(define (lookup-uncat) `(lookup-uncat))
