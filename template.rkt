#lang racket/base

(provide transform)
(require racket/format
         racket/string
         racket/list
         racket/match
         racket/contract

         threading

         pollen/core
         pollen/pagetree
         pollen/file
         pollen/setup

         "rkt/meta-utils.rkt"
         "rkt/language-data.rkt"
         "rkt/contracts.rkt"
         "rkt/post-utils.rkt"
         "rkt/tags.rkt"
         "rkt/tag-utils.rkt"
         "rkt/decoders.rkt"
         "rkt/utils/path.rkt"
         "config.rkt")

(define/contract (with-prefix s) (string? . -> . string?)
  (~a path-prefix s))

(define/contract (with-prefix-lang s) (string? . -> . string?)
  (~a path-prefix-lang s))

(define/contract (css url) (string? . -> . content?)
  `(link ([rel "stylesheet"] [type "text/css"] [href ,url])))

(define/contract (internal-css fname) (string? . -> . content?)
  (css (with-prefix fname)))

(define/contract (js url) (string? . -> . content?)
  `(script ([src ,url])))

(define/contract (internal-js fname) (string? . -> . content?)
  (js (with-prefix fname)))

(define/contract (get-meta-type) (-> symbol?)
  (define raw-type (extract-metas 'type))
  (cond
    [raw-type raw-type]
    [(string-prefix? (get-here) "blog/") 'post]
    [else (error 'get-meta-type "unknown meta type with metas: ~s"
                 (current-metas))]))

(define/contract (get-here) (-> string?)
  (symbol->string (path->pagenode (extract-metas 'here-path))))

;; Section: Navbar

(define/contract (navbar-link uri . label) (string? content? ... . -> . content?)
  `(li ([class ,(string-append
                 "nav-item "
                 (if (string-ci=? uri (get-here)) "active" ""))])
       (a ([class "nav-link"]
           [href ,(build-path-string path-prefix-lang uri)]) ,@label)))

(define/contract (navbar-view-source) (-> content?)
  (navbar-icon
   (string-append "https://github.com/sorawee/my-website/blob/master/"
                  (~> (get-here)
                      (build-path (current-project-root) _)
                      get-markup-source
                      rel-path
                      path->string))
   "Pollen source"
   "fas fa-code"))

(define/contract (navbar-languages) (-> content?)
  `(li ([class "nav-item dropdown"])
       (a ([class "nav-link dropdown-toggle"]
           [href "#"]
           [id "navbarDropdown"]
           [role "button"]
           [data-toggle "dropdown"]
           [aria-haspopup "true"]
           [aria-expanded "false"])
          (i ([class "fas fa-language"])))
       (div ([class "dropdown-menu"] [aria-labelledby "navbarDropdown"])
            ,(! (for/list ([language languages])
                  (define the-lang (symbol->string (car language)))
                  `(a ([class ,(~a "dropdown-item " (if (string=? lang the-lang)
                                                        "disabled"
                                                        ""))]
                       [href ,(build-path-string
                               path-prefix
                               the-lang
                               (rel-path (->output-path (extract-metas 'here-path))))])
                      ,(cdr
                        (or (assoc (string->symbol lang) (cdr language))
                            (assoc 'en (cdr language))))))))))

(define/contract (navbar-icon uri title icon-classes)
  (string? string? string? . -> . content?)
  `(li ([class "nav-item"])
       (a ([class "nav-link"]
           [title ,title]
           [href ,uri]
           [data-toggle "tooltip"]
           [data-placement "bottom"])
          (i ([class ,icon-classes])))))

;; End Section

(define/contract (main-content doc) (content? . -> . content?)
  (decoder
   (match (get-meta-type)
     ['post `(@ ,(title (current-title)) ,(make-post (get-here) #:header? #f))]
     [_ doc])))

(define/contract (transform doc) (content? . -> . content?)
  `(html
    ([lang ,lang])
    (head
     (meta ([charset "utf-8"]))
     (title ,(current-title))
     (meta ([name "author"] [content "Sorawee Porncharoenwase"]))
     (meta ([name "keywords"] [content ""])) ;; TODO
     (meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))
     (link ([rel "icon"] [href ,(with-prefix "static/favicon.ico")])) ;; TODO
     (link ([rel "canonical"] [href ""]))

     ;; CSS
     ,(css "https://afeld.github.io/emoji-css/emoji.css")
     ,(css "https://use.fontawesome.com/releases/v5.5.0/css/all.css")
     ,(css "https://fonts.googleapis.com/css?family=Sarabun")
     ,(css "https://fonts.googleapis.com/css?family=Source+Code+Pro")
     ,(internal-css "static/css/app.css")
     ,(internal-css "static/css/pygments.css")

     (link ([rel "alternate"]
            [type "application/atom+xml"]
            [href ,(with-prefix-lang "feed.xml")])))
    (body
     ;; A standard Twitter Bootstrap navbar
     (nav ([class "navbar navbar-expand-md navbar-light bg-light"])
          (div ([class "container"])
               (div ([class "navbar-brand site-info"])
                    (h1 (a ([href ,(with-prefix-lang "index.html")])
                           ,(lang/th "สรวีย์ พรเจริญวาสน์")
                           ,(lang/en "Sorawee Porncharoenwase")))
                    (p "PhD Student at UW CSE"))
               (button ([class "navbar-toggler navbar-toggler-right"]
                        [type "button"]
                        [data-toggle "collapse"]
                        [data-target "#navbar_collapse"]
                        [aria-controls "navbar_collapse"]
                        [aria-expanded "false"]
                        [aria-label "Toggle navigation"])
                       (span ([class "navbar-toggler-icon"])))
               (div ([class "collapse navbar-collapse"]
                     [id "navbar_collapse"])
                    (ul ([class "navbar-nav ml-auto"])
                        ,(navbar-link "index.html" "Home")
                        ,(navbar-link "blog" "Blog")
                        ,(navbar-link "coq-tactics" "Coq" nbsp "Tactics" '(sup "<α"))
                        ,(navbar-icon
                          "mailto:sorawee@cs.washington.edu"
                          "sorawee@cs.washington.edu"
                          "fas fa-envelope")
                        ,(navbar-icon
                          (string-append "https://norfolk.cs.washington.edu/"
                                         "directory/index.php?"
                                         "prev_floor=3&show_room=CSE394")
                          "CSE 394"
                          "fas fa-map-marker-alt")
                        ,(navbar-icon
                          "https://www.github.com/sorawee"
                          "@sorawee"
                          "fab fa-github")
                        ,(navbar-view-source)
                        ,(navbar-languages)))))

     (main ([class "container"])
           (div ([id ,(~a "content-title--" (slug (get-here)))]
                 [class ,(~a "row content-type--" (get-meta-type))])
                ;; Main column
                (div ,(main-content doc)))
           (footer
            (hr)
            (div ([class "float-md-left"])
                 "Site generated by "
                 (a ([href "http://pollenpub.com"]) "Pollen")
                 "/"
                 (a ([href "https://racket-lang.org"]) "Racket"))
            (div ([class "float-md-right"])
                 "© Sorawee Porncharoenwase")))

     ,(js "https://code.jquery.com/jquery-3.3.1.slim.min.js")
     ,(js "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
     ,(js "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.0/clipboard.min.js")
     ,(internal-js "static/js/bootstrap.bundle.min.js")
     ,(internal-js "static/js/script.js")
     ,@(map internal-js (or (extract-metas 'extra-internal-js) '())))))
