#lang racket/base

(require racket/string racket/match racket/function racket/exn
         racket/sequence racket/file racket/set racket/cmdline
         racket/contract
         (for-syntax racket/base)
         web-server/templates
         pollen/core
         pollen/setup
         "pollen.rkt"
         "utils/file.rkt" "utils/pollen-file.rkt" "utils/watch-dir.rkt")

(define current-init? (make-parameter #f))
(define current-publish? (make-parameter #f))

(command-line
 #:once-each
 [("--init") "Regenerate every tags initially"
             (current-init? #t)]
 [("--publish") "Publish mode: regenerate every tags, and do not watch"
             (current-publish? #t)])

(define/contract (get-tags path) (path-string? . -> . (listof symbol?))
  (filter (negate (curry eq? 'DRAFT)) (select-from-metas 'tags (get-metas path))))

(define/contract (build-map) (-> hash?)
  (parameterize ([current-project-root (current-directory)])
    (for/hash ([path (all-posts)])
      (values (path->string path) (get-tags path)))))

(define (generate-index/subpage tag slice page-number)
  (define posts (string-join (map (curry format "\"~a\"") slice)))
  (define dir (build-path "tags" (symbol->string tag)))
  (when (not (directory-exists? dir)) (make-directory dir))
  (define (get-page)
    (cond
      [(zero? page-number) ""]
      [else (number->string page-number)]))
  (with-output-to-file (build-path dir (format "index~a.html.pm" (get-page)))
    #:exists 'replace
    (thunk (display (include-template #:command-char #\$ "tag.template")))))

(define (generate-index tag paths)
  (for ([page-number (in-naturals)]
        [slice (in-slice 10 (reverse (sort paths path<?)))])
    (generate-index/subpage tag slice page-number)))

(define (delete-all-tags)
  (for ([dir (directory-list "tags" #:build? #t)])
    (when (directory-exists? dir) (delete-directory/files dir))))

(define (delete-tag-if-exists tag)
  (define path (build-path "tags" tag))
  (when (directory-exists? path) (delete-directory/files path)))

(define path-map (build-map))

(define (refresh-tag-index)
  (with-output-to-file "tags.txt" #:exists 'replace
    (thunk (write path-map))))


(define (process-tag tag)
  (printf "Regenerating tag ~a\n" tag)
  (define paths (for/list ([(path tags) path-map] #:when (member tag tags)) path))
  (match paths
    ['() (delete-tag-if-exists tag)]
    [_ (generate-index tag paths)]))

(define (on-file-change path type)
  (printf "~s: ~s\n" path type)
  (cond
    [(pollen-post? (path->string path))
     (match type
       ['delete
        (define tags (hash-ref path-map path))
        (set! path-map (hash-remove path-map path))
        (with-output-to-file "tags.txt" #:exists 'replace
          (thunk (write path-map)))
        (for ([tag tags]) (process-tag tag))]
       [(or 'create 'modify)
        (define new-tags (get-tags path))
        (define old-tags (hash-ref path-map path '()))
        (cond
          [(equal? new-tags old-tags) (void)]
          [else
           (set! path-map (hash-set path-map path new-tags))
           (refresh-tag-index)
           (for ([tag (set-symmetric-difference (list->set new-tags)
                                                (list->set old-tags))])
             (process-tag tag))])])]))

;; we might consider "resetting" the map (by building the map afresh) after
;; an exception occurs

(when (or (current-publish?) (current-init?))
  (delete-all-tags)
  (refresh-tag-index)
  (for ([tag (apply set-union (map list->set (cons '() (hash-values path-map))))])
    (process-tag tag)))

(when (current-publish?)
  (exit))

(void
 (begin
   (displayln "Start watching...")
   (watch-directory
    (build-path ".")
    '(file)
    (λ (path type)
      (cond
        [(string-contains? (path->string path) "compiled/") (void)]
        [else (with-handlers ([exn:fail? (compose1 displayln exn->string)])
                (on-file-change path type))]))
    #:rate 1)))

(let loop ()
  (sleep 1000)
  (loop))
