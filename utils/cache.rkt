#lang racket/base

(provide do-cache)
(require racket/file
         racket/list
         racket/function
         pollen/setup
         "utils.rkt")

(define (do-cache f #:file [file "cache.rktd"] #:limit [size 1000] . args)
  (define cache-dir (build-path (current-project-root) "cache"))
  (when (not (directory-exists? cache-dir))
    (make-directory* cache-dir))
  (set! file (build-path-string cache-dir file))
  (define (prune xs) (if (> (length xs) size) (take xs size) xs))
  (define (add-cache current-cache)
    (define result (apply f args))
    (with-output-to-file file #:exists 'replace
      (thunk (write (prune (cons (list args result) current-cache)))))
    result)
  (cond
    [(file-exists? file)
     (define current-cache (with-input-from-file file read))
     (define cache (assoc args current-cache))
     (if cache
         (second cache)
         (add-cache current-cache))]
    [else (add-cache '())]))
