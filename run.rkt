#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/file
         racket/path
         racket/function
         racket/system)

(define (rm-rf p) (delete-directory/files (simple-form-path p) #:must-exist? #f))

(define (main)
  (define publish? (make-parameter #f))
  (define reset? (make-parameter #f))
  (define reset-hard? (make-parameter #f))
  (define lang (make-parameter "all"))

  (command-line
   #:once-each
   [("--reset") "Reset" (reset? #t)]
   [("--reset-hard") "Hard reset (clear safe cache)" (reset-hard? #t) (reset? #t)]
   [("--lang") l "Language" (lang l)]
   [("--publish") "Publish" (publish? #t)])

  (when (reset-hard?)
    (rm-rf "cache"))

  (when (reset?)
    (system "raco pollen reset"))

  (when (and (not (string=? (lang) "all")) (publish?))
    (raise-user-error "When publishing, lang should be all (or unset)"))

  (cond
    [(publish?)
     (system "raco pollen reset")

     (rm-rf "../my-website-build")
     (make-directory* "../my-website-build")
     (system "sass -f --update static/scss:static/css")
     (system "racket tag-manager.rkt")

     (putenv "ENVIRON" "production")

     (putenv "LANG" "en")
     (time (system "raco pollen render -s"))
     (system "raco pollen publish . ../my-website-build/en")

     ;; (system "raco pollen reset")

     ;; (putenv "LANG" "th")
     ;; (time (system "raco pollen render -s"))
     ;; (system "raco pollen publish . ../my-website-build/th")

     (system "mv ../my-website-build/en/static ../my-website-build/")
     (system "mv ../my-website-build/en/redirect/index.html ../my-website-build/")]
    [(not (publish?))
     (printf "Rendering language: ~a\n" (lang))
     (putenv "LANG" (lang))
     (thread (thunk (system "sass --watch static/scss:static/css")))
     (system "raco pollen start")]))

(void (main))
