#lang racket

(require pollen/decode
         txexpr
         (rename-in pollen/unstable/pygments (highlight original-highlight))
         pollen/core
         pollen/file
         libuuid
         xml
         racket/date
         (only-in srfi/13 string-contains))

(provide highlight
         make-highlight-css
         string-contains
         ->markup-source-path
         (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (define poly-targets '(html txt))
  (define command-char #\@))

(define exclusion-mark-attr '(decode "exclude"))
(define (root . items)
  (decode `(decoded-root ,@items)
          #:txexpr-elements-proc decode-paragraphs
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script pre code)
          #:exclude-attrs (list exclusion-mark-attr)))

(define (bquote . xs) `(blockquote ,@xs))

(define (highlight lang . code-original)
  (define store (make-hash))
  (define code
    (for/list ([s code-original])
      (cond
        [(string? s) s]
        [else
         (define new-s (symbol->string (gensym)))
         (hash-set! store new-s s)
         new-s])))
  (define out (apply original-highlight (cons lang code)))

  (define (extract-highlight f out class)
    (match out
      [`(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ,things-in-pre ...)) "\n")))) "\n")
       `(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ((class ,(string-append class #;" tex2jax_process"))) ,@(f things-in-pre))) "\n")))) "\n")]))

  (match lang
    ['racket
     (define left-paren '(span ((class "p")) "("))
     (define right-paren '(span ((class "p")) ")"))
     (define left-bracket '(span ((class "p")) "["))
     (define right-bracket '(span ((class "p")) "]"))
     (define left-thing? (curryr member (list left-paren left-bracket)))
     (define right-thing? (curryr member (list right-paren right-bracket)))
     (define (parenthesize lst)
       (define (normalize lst)
         (match lst
           [(list `(span ((class "p")) ,str) rst ...)
            (append (map (λ (x) `(span ((class "p")) ,(string x))) (string->list str))
                    (normalize rst))]
           [(list fst rst ...) (cons fst (normalize rst))]
           [_ lst]))
      (define-values (parsed-flipped)
        (for/fold ([stack '()]) ([e (normalize lst)])
          (match e
            [(? left-thing? lp) (values (cons lp stack))]
            [(? right-thing? rp)
             (define-values (grouped new-stack) (splitf-at stack (negate left-thing?)))
             (values
               (match new-stack
                 [(list (? left-thing? lp) _ ...)
                  (match (list lp rp)
                    [(list (== left-paren) (== right-paren)) #f]
                    [(list (== left-bracket) (== right-bracket)) #f]
                    [_ (error 'mismatched-type-paren)])
                  (cons `(span [[class "paren"]]
                               ,@(reverse (append (list e) grouped (list lp))))
                        (rest new-stack))]
                 [_ (cons e stack)]))] ; if too many right parentheses
            [_ (values (cons e stack))])))
      (reverse parsed-flipped))
      (extract-highlight (compose1 parenthesize) out "racket")]
    [_ (extract-highlight (lambda (lst)
        (for/list ([x lst])
          (cond
            [(and (list? x) (hash-has-key? store (last x)))(println (hash-ref store (last x))) (hash-ref store (last x))]
            [else x]))) out "other")]))

(define see-more `(see-more))

(define super-title "Sorawee's Website")
(define (! lst) `(@ ,@lst))

(define (pdfable? file-path)
  (string-contains? file-path ".poly"))

(define (pdfname page) (string-replace (path->string (file-name-from-path page))
                                       "poly.pm" "pdf"))

(define (margin-note . text)
  (define refid (uuid-generate))
  `(@ (label [[for ,refid] [class "margin-toggle"]] 8853)
      (input [[type "checkbox"] [id ,refid] [class "margin-toggle"]])
      (span [[class "marginnote"]] ,@text)))

(define (numbered-note . text)
  (define refid (uuid-generate))
  `(@ (label [[for ,refid] [class "margin-toggle sidenote-number"]])
      (input [[type "checkbox"] [id ,refid] [class "margin-toggle"]])
      (span [[class "sidenote"]] ,@text)))

(define tags-ending-summary '(see-more))
(define tags-skip '(label input))
(define class-skip '("sidenote"))
(define (get-summary tx-in)
  (define found #f)
  (define (iter x)
    (cond
      [(txexpr? x)
       (let-values ([(tag attrs elements) (txexpr->values x)])
         (cond
           [(member tag tags-ending-summary) (begin (set! found #t) #f)]
           [(or (member tag tags-skip)
                (ormap (curryr member class-skip)
                       (string-split (second (or (assoc 'class attrs) '("" "")))
                                     " "))) ""]
           [else
            (let-values ([(elements)
              (for/fold ([acc-elements empty]) ([ele elements]) #:break found
                (define item (iter ele))
                (values (if (or (not found) item)
                            (cons item acc-elements)
                            acc-elements)))])
              (make-txexpr tag attrs (reverse elements)))]))]
      [else x]))
  (define tx-out (iter tx-in)) ; evaluate (iter tx-in) first to set found
  (and found (splice-top tx-out)))

(define (kbds s)
  (! (add-between (map (λ (x) `(kbd ,x)) (string-split s " ")) " + ")))

(define (ids . s) s)

; From https://afeld.github.io/emoji-css/
(define (emj s)
  (define class (match s
    [":P" "em-stuck_out_tongue"]
    [":)" "em-smiley"]
    [":(" "em-disappointed"]
    [":D" "em-grinning"]))
  `(i ((class ,(string-append "em " class)))))

(define (edited-on . xs) (! (list '(br) (apply emph (cons "Edited on " xs)))))

(define (link url . texts) `(a ((href ,url))
  ,(match texts
    ['() url]
    [_ (! texts)])))


(define (LaTeX-transform s)
  (define table '(("<->" "\\leftrightarrow")
                  ("|->" "\\mapsto")
                  ("\\->" "\\to")
                  ("\\<-" "\\leftarrow")
                  ("|=" "\\lmodels")
                  ("|-" "\\vdash")))
  (foldl (λ (item prev) (string-replace prev (first item) (second item))) s table))

(define (msplice xs)
  (define (splice-one x)
    (match (msplice x)
      [(list '@ xs ...) xs]
      [y (list y)]))
  (match xs
    [(list xs ...) (apply append (map splice-one xs))]
    [_ xs]))

(define ($ . xs)
  `(mathjax ,(! `("$" ,@(map LaTeX-transform (msplice xs)) "$"))))
(define ($$ . xs)
  `(mathjax ,(! `("$$" ,@xs "$$"))))

(define (proof #:wrong [wrong #f] . xs) `(div [[class "proof"]] (span [[class "subproof"]] (i "Proof. ") ,@xs) ))
(define (lemma . xs) `(div [[class "lemma"]] (b "Lemma ") ,@xs))

(define (pre #:options options . xs)
  (define hash #hasheq((allowed-math . "tex2jax_process")))
  (define (get-class options)
    (string-join (filter-map (λ (option) (hash-ref hash option #f)) options)))
  `(pre [[class ,(get-class options)]] ,@xs))

(define (item . items) `(li ,@items))
(define (is-item x)
  (match x
    [`(li ,_ ...) x]
    [_ (error "not-item-in-list" x)]))

(define (numberlist . items) `(ol ,@(map is-item items)))
(define (itemlist . items) `(ul ,@(map is-item items)))
(define (argumentlist . items)
  (define-values (premises conclusion) (split-at (map is-item items) (sub1 (length items  ))))
  (match-define `((li ,xs ...))  conclusion)
  `(div [[class "argument"]] (ol [[style "margin-bottom: 0;"]] ,@premises) (ul [[class "no-bullet"]] (li (hr [[class "logic-separator"]])) (li [[class "logic-conclusion"]] ,@xs))))

(define (emph . xs) `(i ,@xs))
(define (bold . xs) `(b ,@xs))
(define (under . xs) `(u ,@xs))
(define (strike . xs) `(s ,@xs))
(define (code . xs) `(code ,@xs))
(define (tt . xs) `(tt ,@xs))

(define (outdated . xs)
  `(div [[class "outdated"]]
        (div (u (b "Outdated:")))
        (div ,@xs)))

(define (menu s)
  (! (add-between (map (λ (x) `(b ,x)) (string-split s " > ")) " > ")))

(define (LaTeX) `(span [[class "latex"]]
                       "L"
                       (span [[class "latex-sup"]] "a")
                       "T"
                       (span [[class "latex-sub"]] "e")
                       "X"))

(define (img path #:width [width "40%"])
  `(img [[src ,path] [style ,(string-append "width:" width)]]))

(define (figure path #:width [width "40%"] #:caption [caption '()])
  `(center ,(img path #:width width) (br) ,@caption))

(define (task . xs) `(u ,@xs))

(define (notice . xs) `(i ,@xs))
(define (migration-notice) (notice "This is a post migrated from my old blog."))

(define (eng . xs) (! (append (list "(ภาษาอังกฤษ: ") xs (list ")"))))

(define (^ . xs) `(sup ,@xs))

(define filebox-tag 'div)
(define filebox-class "filebox")
(define filename-tag 'div)
(define filename-class "filename")
(define (filebox filename . xs)
  `(,filebox-tag [[class ,filebox-class]]
                 (,filename-tag [[class ,filename-class] ,exclusion-mark-attr]
                                ,(format "~a" filename))
                 ,@xs))

(define (filebox-highlight filename lang . xs)
  (filebox filename (apply highlight lang xs)))

(define (splice-top doc) (! (get-elements doc)))

; Copied from try-pollen
; Modified from https://github.com/malcolmstill/mstill.io/blob/master/blog/pollen.rkt
(define (filename->date datetime)
  (match (regexp-match #rx"^blog/_?(....)-(..)-(..)-" datetime)
    [(list _ year month day)
     (seconds->date
       (apply find-seconds (append (list 0 0 0)
                                   (map string->number
                                        (list day month year)))))]
    [_ #f]))

(define (make-post post
                   #:see-more [see-more #f]
                   #:header [header #t])
  (define content ((if see-more get-summary splice-top) (get-doc post)))
  (define name (string->symbol
    (regexp-replace #px"(.*?)\\.poly\\.pm"
                    (symbol->string post) "\\1.html")))
  (define (get-see-more)
    `(div [[class "see-more-link"]]
          (a [[href ,(string-append "/" (symbol->string name))]]
             (span [[class "smallcaps"]] "[see more]"))))
  (define/contract (make-tag tag)
    (-> string? any/c)
    `(a [[href ,(string-append "/tags/" tag)]] ,tag))
  (! `(,(if header
            `(h2 (a [[href ,(string-append "/" (symbol->string name))]]
                    (span [[class "smallcaps"]] ,(select-from-metas 'title name))))
               "")
       (p [[class "tags"]]
          "Tags: "
          ,@(add-between (map make-tag
                              (map symbol->string
                              (select-from-metas 'tags name)))
                         ", "))
       (p [[class "pubdate"]]
          "Published on " ,(date->string (filename->date (symbol->string name))))
       ,(if content `(div [[class "content"]] ,content) "")
       ,(if see-more (get-see-more) "")
       (hr))))

(define (button id title)
  `(button [[id ,id]] ,title))

(define (canvas id)
  `(table (tr (td (canvas [[id ,id] [width "560"] [height "200"]])))))

(define (folded title . xs)
  (define refid (uuid-generate))
  `(@ (label [[for ,refid] [class "fold-toggle"]] ,title)
      (input [[type "checkbox"] [id ,refid] [class "fold-toggle"]])
      (span [[class "folded-content"]] ,@xs)))

(define (spoiler . xs)
  (apply folded (cons "[spoiler ⊕]" xs)))

#|
  From try-pollen

  A slightly smarter version of ->markup-source-path. A file listed as
  "page.html" in a pagetree might have a source page.html.pm, but it might
  instead have a source "page.poly.pm". This function tests for the existence
  of the .html.pm version; if that fails, the .poly.pm version is returned.
|#
(define (get-markup-source str)
  (let* ([default-source (->markup-source-path str)])
    (if (file-exists? default-source)
        default-source
        (string->path (string-replace (path->string default-source) ".html" ".poly")))))
