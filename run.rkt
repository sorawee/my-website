#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/file
         racket/path
         racket/function
         racket/system)

(define (main)
  (define publish? (make-parameter #f))
  (define reset? (make-parameter #f))

  (command-line
   #:once-each
   [("--reset") "Reset" (reset? #t)]
   [("--publish") "Publish" (publish? #t)])

  (when (reset?)
    (delete-file "cache/cache.rktd")
    (delete-file "cache/coq.rktd")
    (system "raco pollen reset"))

  (cond
    [(publish?)
     (putenv "ENVIRON" "production")
     (delete-directory/files (simple-form-path "../my-website-build")
                             #:must-exist? #t)
     (system "racket tag-manager.rkt --publish")
     (system "raco pollen render -s")
     (system "sass scss:css")
     (system "raco pollen publish . ../my-website-build")]
    [(not (publish?))
     (thread (thunk (system "racket tag-manager.rkt")))
     (thread (thunk (system "sass --watch scss:css")))
     (system "raco pollen start")]))

(void (main))
