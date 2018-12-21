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
  (define lang (make-parameter "all"))

  (command-line
   #:once-each
   [("--reset") "Reset" (reset? #t)]
   [("--lang") l "Language" (lang l)]
   [("--publish") "Publish" (publish? #t)])

  (when (reset?)
    (rm-rf "cache/cache.rktd")
    (rm-rf "cache/coq.rktd")
    (system "raco pollen reset"))

  (cond
    [(publish?)
     (putenv "ENVIRON" "production")
     (putenv "LANG" "eng") ;; TODO: for now, since Thai language is not ready yet
     (rm-rf "../my-website-build")
     (system "racket tag-manager.rkt --publish")
     (system "raco pollen render -s")
     (system "sass -f --update scss:css")
     (system "raco pollen publish . ../my-website-build")]
    [(not (publish?))
     (printf "Rendering language: ~a\n" (lang))
     (putenv "LANG" (lang))
     (thread (thunk (system "racket tag-manager.rkt")))
     (thread (thunk (system "sass --watch scss:css")))
     (system "raco pollen start")]))

(void (main))
