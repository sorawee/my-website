;; Patch from https://github.com/otherjoel/try-pollen/blob/master/feed.xml.pp

#lang racket/base

(provide (all-from-out pollen/file)
         get-markup-source)

(require racket/string
         (except-in pollen/file get-markup-source))

;; A slightly smarter version of ->markup-source-path. A file listed as
;; "page.html" in a pagetree might have a source page.html.pm, but it might
;; instead have a source "page.poly.pm". This function tests for the existence
;; of the .html.pm version; if that fails, the .poly.pm version is returned.
(define (get-markup-source str)
  (let* ([default-source (->markup-source-path str)])
    (if (file-exists? default-source)
        default-source
        (string->path (string-replace (path->string default-source)
                                      ".html" ".poly")))))
