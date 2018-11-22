#lang racket/base

;; This file is largely copied from
;; https://github.com/otherjoel/try-pollen/blob/master/feed.xml.pp

#|
  Pollen preprocessor file to generate a valid Atom 1.0 XML feed from a given
  ptree. Based heavily on sample code sent to me by Matthew Butterick, with the
  following minor changes:
   - Use Atom instead of RSS 2.0, and RFC 3339 dates
   - Allow additional filtering to determine which pages should be included
   - Allow poly sources to be loaded if .html.pm source does not exist
   - Add a call to (flatten) to support nested pagetrees
   -
|#


(require racket/contract
         racket/date
         racket/format
         racket/string
         racket/function
         xml
         pollen/core
         pollen/template)

#|
  Really only need datestring->date from here. If you wanted to make this file
  entirely self-contained you could copy/paste that function's definition here.
  This is the function that interprets datestrings in the markup sources, in
  this case in the format "yyyy-mm-dd" or "yyyy-mm-dd hh:mm".
|#
(require (only-in "pollen.rkt" get-summary))
(require (only-in "utils/file.rkt" post-filename->date all-posts))
(require (only-in "utils/utils.rkt" list->date))
(require (only-in "utils/pollen-file.rkt" get-markup-source))

#|
  Customizeable values
|#

(define opt-feed-ptree   "index.ptree")
(define opt-author-name  "Sorawee Porncharoenwase")
(define opt-author-email "sorawee@cs.washington.edu")
(define opt-feed-title   "Sorawee Porncharoenwase")
(define opt-feed-site    "https://homes.cs.washington.edu/~sorawee/") ; This should end with /

#|
  You should customize the timezone/DST settings to match those of the
  computer that will be generating this feed.
|#
(define opt-feed-timezone -8)
(define adjust-daylight-savings? #t)

#|
  This is where you’d normally be told not to change anything below this point.
  But you are expected to know how this works and change it if you need to!
|#

#|
  Defining a struct generates "accessor" functions for each of the struct's
  elements.
|#
(struct rss-item (title author link summary pubdate update) #:transparent)

(define (as-cdata string)
  (cdata #f #f (format "<![CDATA[~a]]>" string)))

(define (email-encode str)
  (map char->integer (string->list str)))

; Atom feeds require dates to be in RFC 3339 format
(define (date->rfc3339 d)
  (string-append (format "~a-~a-~aT~a:~a:~a~a:00"
                         (date-year d)
                         (~r (date-month d) #:min-width 2 #:pad-string "0")
                         (~r (date-day d) #:min-width 2 #:pad-string "0")
                         (~r (date-hour d) #:min-width 2 #:pad-string "0")
                         (~r (date-minute d) #:min-width 2 #:pad-string "0")
                         (~r (date-second d) #:min-width 2 #:pad-string "0")
                         (if (and (date-dst? d) adjust-daylight-savings?)
                             (~r (+ 1 opt-feed-timezone) #:min-width 2 #:pad-string "0" #:sign '++)
                             (~r opt-feed-timezone #:min-width 2 #:pad-string "0" #:sign '++)))))

#|
  make-rss
  This function builds a complete X-expression representation of an RSS feed using
  the Atom 1.0 format.
|#
(define/contract (make-feed-xexpr title link rss-items)
  (string? string? (listof rss-item?) . -> . xexpr/c)
  (define items
    (for/list ([ri (in-list rss-items)])
      (define item-url (string-append* (list link (rss-item-link ri))))
      `(entry
        (author (name ,(rss-item-author ri)))
        (published ,(date->rfc3339 (rss-item-pubdate ri)))
        (updated ,(date->rfc3339 (rss-item-update ri)))
        (title [[type "text"]] ,(rss-item-title ri))
        (link [[rel "alternate"] [href ,item-url]])
        (id ,item-url)
        (summary [[type "html"]]
                 ,(as-cdata (string-append
                             (rss-item-summary ri)
                             (xexpr->string `(p (a ((href ,item-url))
                                                   "Click here to read "
                                                   (i ,(rss-item-title ri)))))))))))

  `(feed [[xml:lang "en-us"] [xmlns "http://www.w3.org/2005/Atom"]]
         (title ,title)
         (link [[rel "self"] [href ,link]])
         (generator [[uri "http://pollenpub.com/"]] "Pollen (custom feed)")
         (id ,link)
         (updated ,(date->rfc3339 (current-date)))
         (author
          (name ,opt-author-name)
          (email ,@(email-encode opt-author-email)))
         ,@items))


(define (feed-item-structs)
  (define rss-items (map path->string (all-posts)))
  (define rss-unsorted-item-structs
    (map (λ (item-link)
           (define item-path (get-markup-source item-link))
           (define item-metas (get-metas item-path 'metas))
           (define item-title (select-from-metas 'title item-metas))
           (define item-content (get-doc item-path))
           (define item-author opt-author-name)
           (define item-summary (->html (or (get-summary item-content)
                                            '(p "(No summary given)"))))
           (define item-pubdate (or (post-filename->date (path->string item-path))
                                    (select-from-metas 'published item-metas)))
           (define item-update (or (list->date (hash-ref item-metas 'updated #f))
                                   item-pubdate))
           (when (not item-pubdate) (printf "~a does not have pubdate" item-title))
           (rss-item item-title item-author item-link item-summary item-pubdate item-update))
         rss-items))
    ;; sort from latest to earliest. Doesn't rely on order in ptree file, but rather pub date in source.
    (sort rss-unsorted-item-structs > #:key (λ(i) (date->seconds (rss-item-pubdate i)))))

; Generates a string for the whole RSS feed
(define/contract (complete-feed rss-xexpr)
  (xexpr? . -> . string?)
  (string-append "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                 (xexpr->string rss-xexpr)))

#|
  Now we snap it all together. Because this file uses "lang racket" instead of
  "lang pollen" we have to manually take some steps that Pollen would normally
  take for us: defining 'doc and 'metas, and displaying the output (which will
  be stored in the target file).
|#
(provide doc metas)
(define rss-xpr (make-feed-xexpr opt-feed-title opt-feed-site (feed-item-structs)))
(define doc (complete-feed rss-xpr))
(define metas (hash))
(module+ main (display doc))
