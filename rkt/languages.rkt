#lang racket/base

(require racket/match
         "tag-utils.rkt"
         "../config.rkt"
         (for-syntax racket/base
                     racket/list
                     threading
                     "../config.rkt"
                     "language-data.rkt"))

(define-syntax (lang-id-fun stx)
  (syntax-case stx ()
    [(_ LANG-IDS LANG)
     #`(define (LANG-IDS . xs)
         #,(if (or (string=? lang "all") (string=? lang (syntax->datum #'LANG)))
               #'(! xs)
               #'(! '())))]))

(define-syntax (make-languages stx)
  (let* ([langs (map (λ~> car symbol->string) languages)]
         [lang-ids (map (λ~> (string-append "lang/" _) string->symbol) langs)]
         [in-ids (map (λ~> (string-append "in/" _) string->symbol) langs)]
         [native (map (λ~> cdr
                           (assoc (string->symbol lang) _)
                           (or _ '(all "all"))
                           cdr) languages)])
    (with-syntax ([(LANGS ...) langs]
                  [(LANG-IDS ...) lang-ids]
                  [(IN-IDS ...) in-ids]
                  [(NATIVE ...) native])
      #'(begin
          (provide LANG-IDS ... IN-IDS ...)
          (lang-id-fun LANG-IDS LANGS) ...
          (define (IN-IDS . xs) `(@ "(" #;NATIVE #;": " ,@xs ")")) ...))))

(make-languages)
