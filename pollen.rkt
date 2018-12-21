#lang racket/base

;; let's be very strict about what should be exported

(require (only-in "rkt/decoders.rkt" decoder)

         ;; for providing
         "rkt/tags.rkt"       ;; provide all
         "rkt/languages.rkt"  ;; provide all
         "rkt/meta-utils.rkt" ;; provide all
         "rkt/toplevel.rkt"   ;; provide all

         (only-in racket/function curry)

         (only-in "rkt/mark.rkt" mark)
         (only-in "rkt/tag-utils.rkt" ! !!)
         (only-in "rkt/post-utils.rkt" make-post))

(provide curry
         root

         make-post ;; from rkt/post-utils.rkt
         ! !!      ;; from rkt/tag-utils.rkt
         mark      ;; from rkt/mark.rkt
         (all-from-out "rkt/tags.rkt")
         (all-from-out "rkt/languages.rkt")
         (all-from-out "rkt/meta-utils.rkt")
         (all-from-out "rkt/toplevel.rkt"))

;; the standard Pollen setup
(module setup racket/base
  (provide (all-defined-out))
  (require racket/function
           racket/string
           syntax/modresolve)

  ;; do not allow silent unbound ids
  (define allow-unbound-ids #f)

  ;; make changes in these files refreshable
  (define watchlist '("index-page.rkt"
                      "template.rkt"
                      "rkt/tags.rkt"))
  (define cache-watchlist (map resolve-module-path watchlist))

  ;; setup omitted paths
  (define (omitted-path? p)
    (ormap (curry string-contains? (path->string p))
           '("/cache" "/node_module" "/utils" "/rkt" "/scss" "/.")))

  ;; use @ as the command char
  (define command-char #\@))

;; Voodoo magic: I find that Pollen's templating system is not flexible enough.
;; In particular, the fact that templates are not modules make organization cumbersome.
;; It's also not easy to make nested templates.
;;
;; My approach does most stuff in the Pollen markup form. In addition to being able
;; to return an X-expression which will be rendered directly, the approach allows
;; us to return the following special form X-expression:
;;
;;     (@@app <f> <doc>)
;;
;; where <f> is a path to a file that has a function `transform` defined and <doc>
;; is the usual doc.
;;
;; I also define a function `interp` which takes in the above X-expression
;; and interpret it. If the X-expression is not the special form, then it simply
;; returns it. Otherwise, it calls the function `transform` which is supposed to be
;; a function from X-expression to X-expression and returns the result.
;; (`interp` is a part of `toplevel` in rkt/toplevel.rkt)
;;
;; Note one nifty trick: a `transform` function could return the special form too!
;; This allows nested templating.
;;
;; Convention for X-expression's tag naming:
;; 1. Regular name: valid HTML tags
;; 2. Name starting with @: a tag that should eventually be decoded
;; 3. Name starting with @@: a command tag (like the @@app above)

(define (root . items) `(@@app "template.rkt" ,(! items)))
