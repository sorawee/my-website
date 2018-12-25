#lang racket/base

(provide do-cache cleanup)
(require racket/file
         racket/function
         pollen/setup
         "path.rkt")

(define cache-dir (build-path (current-project-root) "cache"))
(define cache (make-hash))
(define changed? (make-hash))

(define (cleanup)
  (when (not (directory-exists? cache-dir))
    (make-directory* cache-dir))
  (for ([(fname the-cache) cache])
    (when (hash-has-key? changed? fname)
      (printf "Writing ~a\n" fname)
      (with-output-to-file (build-path-string cache-dir fname) #:exists 'replace
        (thunk (write (hash->list the-cache)))))))

(define (do-cache base-proc #:file [fname "cache.rktd"])
  (define path (build-path-string cache-dir fname))
  (cond
    ;; if we have it in the cache already, no need to initialize
    [(hash-has-key? cache fname) (void)]
    ;; if the cachefile exists, load it
    [(file-exists? path)
     (hash-set! cache fname (make-hash (with-input-from-file path read)))]
    ;; otherwise, initialize it to empty locally
    [else (hash-set! cache fname (make-hash))])

  (define the-cache (hash-ref cache fname))

  (make-keyword-procedure
   (λ (kws kw-args . args)
     (hash-ref! the-cache
                (list* kws kw-args args)
                (thunk
                 (hash-set! changed? fname #t)
                 (keyword-apply base-proc kws kw-args args))))))
