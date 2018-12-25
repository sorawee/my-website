#lang racket/base

(provide squote dquote mvar see-more link emj section+link margin-note kbds
         $ $$ script menu filebox-highlight img output text

         define-term term

         nbsp apos

         highlight ;; from highlight.rkt
         (all-from-out "languages.rkt"))

(require racket/contract
         racket/match
         racket/list
         racket/string
         racket/format
         racket/function
         racket/lazy-require
         threading
         pollen/decode
         pollen/tag
         txexpr
         (for-syntax racket/base)
         "contracts.rkt"
         "decoders.rkt"
         "clipboard.rkt"
         "tag-utils.rkt"
         "languages.rkt"
         "utils/string.rkt"
         "utils/path.rkt"
         "../config.rkt")

(lazy-require ["highlight.rkt" (highlight)])

(define terms (make-hash))
(define (define-term s . ts) (hash-set! terms s (! ts)))
(define (term s #:defn [defn #f] #:link [uri #f])
  (define t (hash-ref terms s))
  (cond
    [(and defn uri) `(@ (a ([href ,uri]) (dfn ,t)) " (" ,s ")")]
    [defn `(@ (dfn ,t) " (" ,s ")")]
    [uri `(a ([href ,uri]) ,t)]
    [else t]))

(define-syntax (define+provide-tag stx)
  (syntax-case stx ()
    [(_ id args ...)
     #'(begin
         (provide id)
         (define id (default-tag-function args ...)))]))

(define-syntax (make-tags stx)
  (syntax-case stx ()
    [(_ tags ...) #'(begin (define+provide-tag tags 'tags) ...)]))

(make-tags code blockquote strong div)

(define nbsp 160)
(define apos "’")

(define+provide-tag emph 'em)
(define+provide-tag italic 'i)
(define+provide-tag strike 's)
(define+provide-tag title 'h1)
(define+provide-tag section 'h2)
(define+provide-tag subsection 'h3)
(define+provide-tag subsubsection 'h4)
(define+provide-tag item 'li)
(define+provide-tag orderedlist 'ol)
(define+provide-tag itemlist 'ul)
(define+provide-tag button 'button #:type "button")

(define (text classname . xs)
  `(span ([class ,(~a "text-type-" classname)]) ,@xs))

(define output
  (let ([refid 0])
    (λ xs
      (set! refid (add1 refid))
      `(div ([class "highlight-container"])
            (div ([class "highlight"]) (pre ([id ,(~a "output-id-" refid)]) ,@xs))
            ,(copy-button "output-id" refid)))))

(define/contract (img path #:width [width "40%"] #:class [classname ""])
  ((string?) (#:width string? #:class string?) . ->* . content?)

  `(img ([src ,(build-path-string path-prefix path)]
         [class ,(~a "block-image " classname)]
         [style ,(~a "width: " width)])))

(define (script #:src [src #f] . xs)
  (if src `(script ([src ,src])) `(script ,@xs)))

(define see-more `(@see-more))
(define (mvar . xs) (! (append '("‹") xs '("›"))))

(define/contract (link url . texts) (string? content? ... . -> . content?)
  `(a ([href ,url])
      ,@(match texts
          ['() (list url)]
          [_ texts])))

;; Emoji from https://afeld.github.io/emoji-css/
(define/contract (emj s) (string? . -> . content?)
  (define class (match s
                  [":P" "em-stuck_out_tongue"]
                  [":)" "em-smiley"]
                  [":(" "em-disappointed"]
                  [":D" "em-grinning"]
                  [else (error 'emj "Unrecognized emoji: ~a" s)]))
  `(i ([class ,(~a "em " class)])))

(define (section+link sec li) `(h2 ,sec (span "[" ,li "]")))


;; if the first element is a paragraph tag, splice the content out.
;; This is to make the paragraph and the margin note number in the margin note
;; stay together.
(define/contract (strip-first-paragraph xs) (list? . -> . list?)
  (match xs
    [(list (txexpr 'p _ elems) rst ...) `(,@elems ,@rst)]
    [_ xs]))

(module+ test
  (require rackunit)
  (check-equal?
   (strip-first-paragraph `((p ([class "blah"]) "hello" "\n" "world")
                            (ul (li "a") (li "b"))
                            (p "blah")))
   `("hello" "\n" "world" (ul (li "a") (li "b")) (p "blah"))))

;; create a margin note
(define margin-note
  (let ([refid 0])
    (λ xs
      (set! refid (add1 refid))
      `(@ (label ([for ,(format "margin-note-~a" refid)]
                  [class "margin-note-toggle margin-note-number"]))
          (input ([type "checkbox"]
                  [id ,(format "margin-note-~a" refid)]
                  [class "margin-note-toggle"]))
          (object ([class "margin-note"])
                  (div ([class "margin-note-box"]
                        ,exclusion-mark-attr-paragraph)
                       ,@(~> (! xs)
                             (decode _ #:txexpr-elements-proc decode-paragraphs)
                             rest
                             strip-first-paragraph)))))))

;; create keyboard input elements
(define/contract (kbds s) (string? . -> . content?)
  (! (add-between (map (λ (x) `(kbd ,x)) (string-split s " ")) " + ")))

(module+ test
  (check-equal?
   (kbds "ctrl shift s")
   `(@ (kbd "ctrl")
       " + "
       (kbd "shift")
       " + "
       (kbd "s"))))

;; support non-standard LaTeX commands
(define/contract (LaTeX-transform s) (string? . -> . string?)
  (define table '(("<->"  . "\\leftrightarrow")
                  ("|->"  . "\\mapsto")
                  ("\\->" . "\\to")
                  ("\\<-" . "\\leftarrow")
                  ("|="   . "\\lmodels")
                  ("|-"   . "\\vdash")))
  (string-replace/all table s))

(module+ test
  (check-equal?
   (LaTeX-transform "\\set{P <-> Q, P} |- Q")
   "\\set{P \\leftrightarrow Q, P} \\vdash Q"))

;; make LaTeX elements
(define/contract (make-mathjax-tag t) (string? . -> . (content? ... . -> . content?))
  (λ xs (decode `(@ ,t ,@xs ,t) #:string-proc LaTeX-transform)))

(define $ (make-mathjax-tag "$"))
(define $$ (make-mathjax-tag "$$"))

;; make menu tag
(define (menu s)
  (! (add-between (map (curry list 'b) (string-split s " > ")) " > ")))

(module+ test
  (check-equal? (menu "a > b > c")
                '(@ (b "a") " > " (b "b") " > " (b "c"))))

(define/contract (filebox-highlight filename lang . xs)
  (string? symbol? content? ... . -> . content?)

  `(div ([class "filebox"])
        (div ([class "filename"]) ,filename)
        ,(apply highlight lang xs)))

(define (dquote . xs) `(@ "“" ,@xs "”"))
(define (squote . xs) `(@ "‘" ,@xs "’"))
