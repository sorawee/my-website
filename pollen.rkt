#lang racket/base

(require racket/date
         racket/string
         racket/path
         racket/format
         racket/match
         racket/list
         racket/function
         racket/contract
         racket/contract/region
         racket/file
         (for-syntax racket/base)

         xml

         pollen/core
         pollen/decode
         pollen/setup
         (rename-in pollen/unstable/pygments [highlight original-highlight])

         rackjure/threading
         txexpr
         with-cache

         "utils/utils.rkt"
         "utils/file.rkt"
         "utils/pollen-file.rkt" ;; patched pollen/file
         "utils/slug.rkt"
         "utils/doc-uri.rkt"

         "config.rkt")


(provide take
         drop
         match
         define/match
         empty?
         empty
         curryr
         ~s
         slug
         path-prefix
         (all-defined-out))

(module setup racket/base
  (provide (all-defined-out))
  (require racket/function
           racket/string)
  (define allow-unbound-ids #f)
  (define (omitted-path? p)
    (cond
      [(ormap (curry string-contains? (path->string p))
              '("/cache" "/node_module" "/utils" "/scss" "/."))
       (printf "Omit: ~a\n" p)
       #t]
      [else #f]))
  (define command-char #\@))

(define exclusion-mark-attr '(decode "exclude"))
(define (root . items)
  (decode `(decoded-root ,@items)
          #:txexpr-elements-proc decode-paragraphs
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script pre code)
          #:exclude-attrs (list exclusion-mark-attr)))

(define (highlight/core lang code-original)
  (define out (apply original-highlight (cons lang code-original)
                     #:python-executable "python3"))
  (define (extract-highlight f out class)
    (match out
      [`(div ((class "highlight")) (pre (code ,things-in-pre ...)))
       `(div ((class "highlight")) (pre (code ,@(f things-in-pre))))]
      [`(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ,things-in-pre ...)) "\n")))) "\n")
       `(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ((class ,(string-append class #;" tex2jax_process"))) ,@(f things-in-pre))) "\n")))) "\n")]))

  (match lang
    ['racket
     (define left-paren? (curry equal? '(span ((class "p")) "(")))
     (define right-paren? (curry equal? '(span ((class "p")) ")")))
     (define left-bracket? (curry equal? '(span ((class "p")) "[")))
     (define right-bracket? (curry equal? '(span ((class "p")) "]")))
     (define left-focus?
       (match-lambda [`(span ([class "n"]) ,s) (string-prefix? s "⟦")] [_ #f]))
     (define right-focus?
       (match-lambda [`(span ([class "n"]) ,s) (string-prefix? s "⟧")] [_ #f]))
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
      (extract-highlight (compose1 parenthesize) out "racket")]
    [_ (extract-highlight values out "other")]))

(define (highlight lang . code-original)
  (do-cache highlight/core lang code-original))

(define (do-cache f #:file [file "cache.rktd"] #:limit [size 100] . args)
  (define cache-dir (build-path (current-project-root) "cache"))
  (when (not (directory-exists? cache-dir))
    (make-directory* cache-dir))
  (set! file (build-path-string cache-dir file))
  (define (prune xs) (if (> (length xs) size) (take xs size) xs))
  (define (add-cache current-cache)
    (define result (apply f args))
    (with-output-to-file file #:exists 'replace
      (thunk (write (prune (cons (list args result) current-cache)))))
    result)
  (cond
    [(file-exists? file)
     (define current-cache (with-input-from-file file read))
     (define cache (assoc args current-cache))
     (if cache
         (second cache)
         (add-cache current-cache))]
    [else (add-cache '())]))


(define super-title "Sorawee's Website")
(define (! lst) `(@ ,@lst))

(define (pdfable? file-path)
  (string-contains? file-path ".poly"))

(define (pdfname page) (string-replace (path->string (file-name-from-path page))
                                       "poly.pm" "pdf"))

(define refid 0)

(define (margin-note . xs)
  (set! refid (add1 refid))
  `(@ (label ([for ,(format "margin-note-~a" refid)]
              [class "margin-note-toggle margin-note-number"]))
      (input ([type "checkbox"]
              [id ,(format "margin-note-~a" refid)]
              [class "margin-note-toggle"]))
      (span ([class "margin-note"]) ,@xs)))


(define (kbds s)
  `(@ ,@(add-between (map (λ (x) `(kbd ,x)) (string-split s " ")) " + ")))

(define (ids . s) s)

(define (edited-on . xs) `((br) (i "Edited on " ,@xs)))



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

(define ($ . xs) `(mathjax "$" ,@(map LaTeX-transform (msplice xs)) "$"))
(define ($$ . xs) `(mathjax "$$" ,@xs "$$"))

(define (proof #:wrong [wrong #f] . xs) `(div [[class "proof"]] (span [[class "subproof"]] (i "Proof. ") ,@xs) ))
(define (lemma . xs) `(div [[class "lemma"]] (b "Lemma ") ,@xs))

(define (pre #:options options . xs)
  (define hash #hasheq((allowed-math . "tex2jax_process")))
  (define (get-class options)
    (string-join (filter-map (λ (option) (hash-ref hash option #f)) options)))
  `(pre [[class ,(get-class options)]] ,@xs))

(define (item? x)
  (match x
    [`(li ,_ ...) #t]
    [_ #f]))

(define/contract (numberlist . items) (item? ... . -> . xexpr/c) `(ol ,@items))
(define/contract (itemlist . items) (item? ... . -> . xexpr/c) `(ul ,@items))

(define/contract (argumentlist . items) (item? ... . -> . xexpr/c)
  (define-values (premises conclusion) (split-at items (sub1 (length items))))
  (match-define `((li ,xs ...))  conclusion)
  `(div [[class "argument"]] (ol [[style "margin-bottom: 0;"]] ,@premises) (ul [[class "no-bullet"]] (li (hr [[class "logic-separator"]])) (li [[class "logic-conclusion"]] ,@xs))))

(define (under . xs) `(u ,@xs))
(define (strike . xs) `(s ,@xs))
(define (tt . xs) `(tt ,@xs))
(define (script #:src [src #f] . xs)
  (if src `(script [[src ,src]]) `(script ,@xs)))
(define (phantom . xs) `(div [[style "display: none"]] ,@xs))

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
(define (migration-notice) (notice "This post is migrated from my old blog."))

(define (eng . xs) (! (append (list "(ภาษาอังกฤษ: ") xs (list ")"))))

(define (^ . xs) `(sup ,@xs))

(define filebox-tag 'div)
(define filebox-class "filebox")
(define filename-tag 'div)
(define filename-class "filename")
(define (filebox filename . xs)
  `(,filebox-tag ([class ,filebox-class])
                 (,filename-tag ([class ,filename-class] ,exclusion-mark-attr)
                                ,(~a filename))
                 ,@xs))

(define (filebox-highlight filename lang . xs)
  (filebox filename (apply highlight lang xs)))

(define (splice-top doc) `(@ ,@(get-elements doc)))

(define/contract (make-tag tag) (string? . -> . xexpr/c)
  `(a [[href ,(build-path-string path-prefix "tags" tag)]] ,tag))

(define/contract (make-post post #:see-more? [see-more? #f] #:header? [header? #t])
  ((path-string?) (#:see-more? boolean? #:header? boolean?) . ->* . xexpr/c)

  (define path (build-path-string (current-project-root) post))
  (printf "Make post with: ~a and ~a\n" post path)
  (define content ((if see-more? get-summary splice-top) (get-doc path)))
  (define post-uri (build-path-string path-prefix (->output-path post)))
  (define/contract (get-see-more) (-> xexpr/c)
    `(div [[class "see-more-link"]]
          (a [[href ,post-uri]]
             (span [[class "smallcaps"]] "[see more]"))))
  (define/contract (get-update-on) (-> (listof xexpr/c))
    (define update-on (hash-ref (get-metas path) 'updated #f))
    (cond
      [update-on
       `((p ([class "update"])
            (span ([class "pubdate"]) "Updated on "
                  ,(date->string (list->date update-on)))))]
      [else '()]))
  `(@ ,(if header? `(h2 (a ([href ,post-uri]) ,(select-from-metas 'title path))) "")
      (p (span ([class "pubdate"]) "Published on "
               ,(date->string (post-filename->date post)))
         " :: "
         (span
          ,@(add-between
             (map make-tag (map symbol->string (select-from-metas 'tags path))) ", ")))
      ,@(get-update-on)
      ,(if content `(div [[class "post-content"]] ,content) "")
      ,(if see-more? (get-see-more) "")))


(define (canvas id)
  `(table (tr (td (canvas [[id ,id] [width "560"] [height "200"]])))))

;; (define (folded title . xs)
;;   (define refid (uuid-generate))
;;   `(@ (label [[for ,refid] [class "fold-toggle"]] ,title)
;;       (input [[type "checkbox"] [id ,refid] [class "fold-toggle"]])
;;       (span [[class "folded-content"]] ,@xs)))

;; (define (spoiler . xs)
;;   (apply folded (cons "[spoiler ⊕]" xs)))




;; make-post is called in three different contexts
;; 1. /tag/tagname/index.html, pollen markup, two levels deep
;; 2. /blog.html             , pollen markup, zero level deep
;; 3. /blog/post.html        , template     , one level deep












;; Section: Tags

(define-syntax (make-tags stx)
  (syntax-case stx ()
    [(_ tags ...) #'(begin (define (tags . xs) `(tags ,@xs)) ...)]))

(make-tags code blockquote strong div)

(define nbsp 160)
(define apos "’")

(define (emph . xs) `(em ,@xs))
(define (italic . xs) `(i ,@xs))
(define (item . items) `(li ,@items))
(define (mvar . xs) (apply string-append (append '("⟪") xs '("⟫"))))

(define (section . xs) `(h2 ,@xs))
(define (subsection . xs) `(h3 ,@xs))
(define (subsubsection . xs) `(h4 ,@xs))

(define see-more `(see-more))

(define/contract (button id title) (string? xexpr/c . -> . xexpr/c)
  `(button [[id ,id]] ,title))

(define/contract (link url . texts) (string? xexpr/c ... . -> . xexpr/c)
  `(a ([href ,url])
      ,@(match texts
          ['() (list url)]
          [_ texts])))

;; Emoji from https://afeld.github.io/emoji-css/
(define (emj s)
  (define class (match s
                  [":P" "em-stuck_out_tongue"]
                  [":)" "em-smiley"]
                  [":(" "em-disappointed"]
                  [":D" "em-grinning"]))
  `(i ((class ,(string-append "em " class)))))

(define (section+link sec li) `(h2 ,sec (span "[" ,li "]")))
(define (publication-list . xs) `(ul ([class "publication-list"]) ,@xs))
(define (publication title paper-link authors conference)
  (item (div (link paper-link title))
        (apply div (add-between (map (λ (x) (if (eq? x 'me)
                                          (strong "Sorawee Porncharoenwase")
                                          x))
                               authors) ", "))
        `(div (i ,conference))))

;; End Section

;; Section: Summary

(define tags-skip '(label input))
(define class-skip '("margin-note"))
(define/contract (get-summary tx-in) (txexpr? . -> . (or/c #f txexpr?))
  (define found #f)
  (define (iter x)
    (cond
      [(txexpr? x)
       (define-values (tag attrs elements) (txexpr->values x))
       (cond
         [(member tag see-more) (set! found #t)
                                #f]
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
            (make-txexpr tag attrs (reverse elements)))])
       ]
      [else x]))
  (define tx-out (iter tx-in)) ; evaluate (iter tx-in) first to set found
  (and found (splice-top tx-out)))

;; End Section


;; Section: Parameters

(define/contract current-here parameter? (make-parameter #f)) ;; (or/c string? #f)

;; End Section

;; Section: Helpers

(define/contract (extract-metas key) (symbol? . -> . any/c)
  (select-from-metas key (current-metas)))

(define/contract (get-meta-type) (-> symbol?)
  (define raw-type (extract-metas 'type))
  (cond
    [raw-type raw-type]
    [(string-prefix? (current-here) "blog/") 'post]
    [else (error 'get-meta-type "unknown meta type with metas: ~s and path: ~s"
                 (current-metas) (current-here))]))

;; End Section

;; Section: Navbar

(define/contract (navbar-link uri label) (string? string? . -> . xexpr/c)
  `(li ([class ,(string-append
                 "nav-item "
                 (if (string-ci=? uri (current-here)) "active" ""))])
       (a ([class "nav-link"] [href ,(build-path-string path-prefix uri)]) ,label)))

(define/contract (navbar-view-source) (-> xexpr/c)
  (navbar-icon
   (string-append "https://github.com/sorawee/my-website/blob/master/"
                  (path->string (get-markup-source (current-here))))
   "Pollen source"
   "fas fa-code"))

(define/contract (navbar-icon uri title icon-classes)
  (string? string? string? . -> . xexpr/c)
  `(li ([class "nav-item"])
       (a ([class "nav-link"]
           [title ,title]
           [href ,uri]
           [data-toggle "tooltip"]
           [data-placement "bottom"])
          (i ([class ,icon-classes])))))

;; End Section

(define/contract (blog-header) (-> xexpr/c)
  `(a ([href ,(build-path-string path-prefix "feed.xml")]
       [class "btn btn-rss"])
      (i ([class "fas fa-rss"]))
      " Subscribe"))

(define/contract (main-content) (-> xexpr/c)
  (match (get-meta-type)
    ['post `(@ (h1 ,(extract-metas 'title)) ,(make-post (current-here) #:header? #f))]
    [_ (get-doc (build-path-string (current-project-root) (current-here)))]))
