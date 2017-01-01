#lang racket

(require pollen/decode
         txexpr
         pollen/unstable/pygments
         pollen/core
         libuuid
         xml
         racket/date
         (only-in srfi/13 string-contains)
         racket/cmdline)

(provide highlight
         make-highlight-css
         string-contains
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

(define (emj s)
  (define class (match s
    [":P" "em-stuck_out_tongue"]
    [":)" "em-smiley"]
    [":(" "em-disappointed"]
    [else `(span ((class "unmatched-emoji")) ,s)]))
  (if (string? s) `(i ((class ,(string-append "em " class)))) s))

(define (link url . texts) `(a ((href ,url))
  ,(match texts
    ['() url]
    [_ (! texts)])))


(define ($ . xs)
  `(mathjax ,(apply string-append `("$" ,@xs "$"))))
(define ($$ . xs)
  `(mathjax ,(apply string-append `("$$" ,@xs "$$"))))


(define (numberlist . items) `(ol ,@items))
(define (itemlist . items) `(ul ,@items))

(define (item . items) `(li ,@items))

(define (emph . xs) `(i ,@xs))

(define (menu s)
  (! (add-between (map (λ (x) `(b ,x)) (string-split s " > ")) " > ")))

(define (LaTeX) `(span [[class "latex"]]
                       "L"
                       (span [[class "latex-sup"]] "a")
                       "T"
                       (span [[class "latex-sub"]] "e")
                       "X"))

(define (img path) `(img ((src ,path))))

(define (task . xs) `(u ,@xs))

(define (notice . xs) `(i ,@xs))
(define (migration-notice) (notice "This is a post migrated from my old blog."))

(define (^ . xs) `(sup ,@xs))

(define filebox-tag 'div)
(define filebox-class "filebox")
(define filename-tag 'div)
(define filename-class "filename")
(define (filebox filename . xs)
  `(,filebox-tag ((class ,filebox-class))
                 (,filename-tag ((class ,filename-class) ,exclusion-mark-attr)
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
                   #:content [content (splice-top (get-doc post))]
                   #:see-more [see-more #f]
                   #:header [header #t])
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
