#lang racket/base

(provide squote dquote emph italic mvar section subsection subsubsection
         see-more button link emj section+link margin-note kbds
         $ $$ item numberlist itemlist strike script menu
         filebox filebox-highlight img title

         highlight ;; from highlight.rkt

         nbsp apos)

(require racket/contract
         racket/match
         racket/list
         racket/string
         racket/format
         racket/function
         pollen/decode
         txexpr
         (for-syntax racket/base)
         "highlight.rkt"
         "contracts.rkt"
         "decoders.rkt"
         "tag-utils.rkt"
         "meta-utils.rkt"
         "utils/string.rkt"
         "utils/path.rkt"
         "../config.rkt")

(define-syntax (make-tags stx)
  (syntax-case stx ()
    [(_ tags ...) #'(begin
                      (provide tags ...)
                      (define (tags . xs) `(tags ,@xs)) ...)]))

(make-tags code blockquote strong div)

(define nbsp 160)
(define apos "’")

(define (emph . xs) `(em ,@xs))
(define (italic . xs) `(i ,@xs))
(define (mvar . xs) (string-append* (append '("‹") xs '("›"))))

(define (section . xs) `(h2 ,@xs))
(define (subsection . xs) `(h3 ,@xs))
(define (subsubsection . xs) `(h4 ,@xs))

(define see-more `(@see-more))

(define/contract (button id title) (string? content? . -> . content?)
  `(button ([id ,id]) ,title))

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
          (object ([class "margin-note"]
                   ,exclusion-mark-attr-paragraph)
                  (div ([class "margin-note-box"])
                       ,@(strip-first-paragraph xs)))))))

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

(define/contract (make-mathjax-tag t) (string? . -> . (content? ... . -> . content?))
  (λ xs (decode `(mathjax ,t ,@xs ,t) #:string-proc LaTeX-transform)))

;; make LaTeX elements
(define $ (make-mathjax-tag "$"))
(define $$ (make-mathjax-tag "$$"))

;; check if the input is a list item
(define/contract (item? x) (any/c . -> . boolean?)
  (match x
    [(txexpr 'li _ _) #t]
    [_ #f]))

;; make lists
(define (item . items) `(li ,@items))
(define (numberlist . items) `(ol ,@items))
(define (itemlist . items) `(ul ,@items))

(define (strike . xs) `(s ,@xs))
(define (script #:src [src #f] . xs)
  (if src `(script ([src ,src])) `(script ,@xs)))

;; make menu tag
(define (menu s)
  (! (add-between (map (curry list 'b) (string-split s " > ")) " > ")))

(module+ test
  (check-equal? (menu "a > b > c")
                '(@ (b "a") " > " (b "b") " > " (b "c"))))


(define/contract (filebox filename . xs) (string? content? ... . -> . content?)
  `(div ([class "filebox"])
        (div ([class "filename"]) ,filename)
        ,@xs))

(define/contract (filebox-highlight filename lang . xs)
  (string? symbol? content? ... . -> . content?)

  (filebox filename (apply highlight lang xs)))

(define/contract (img path #:width [width "40%"] #:class [class ""])
  ((string?) (#:width string? #:class string?) . ->* . content?)

  `(img ([src ,(build-path-string path-prefix
                                  (rel-path (extract-metas 'here-path))
                                  ".."
                                  path)]
         [class ,(string-append "block-image " class)]
         [style ,(string-append "width: " width)])))

(define (title . xs) `(h1 ,@xs))

(define (dquote . xs) `(@ "“" ,@xs "”"))
(define (squote . xs) `(@ "‘" ,@xs "’"))
