#lang racket/base

(require racket/string racket/match racket/function
         racket/sequence racket/file racket/set
         racket/contract
         web-server/templates
         pollen/core
         pollen/setup
         (for-syntax racket/base) ;; to make template rendering go through
         "rkt/post-file-utils.rkt")


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
        [slice (in-slice 10 (reverse (sort (map build-path paths) path<?)))])
    (generate-index/subpage tag slice page-number)))

(define (delete-all-tags)
  (for ([dir (directory-list "tags" #:build? #t)])
    (when (directory-exists? dir) (delete-directory/files dir))))

(define (delete-tag-if-exists tag)
  (define path (build-path "tags" tag))
  (when (directory-exists? path) (delete-directory/files path)))

(define path-map (build-map))

(define (process-tag tag)
  (printf "Regenerating tag ~a\n" tag)
  (define paths (for/list ([(path tags) path-map] #:when (member tag tags)) path))
  (match paths
    ['() (delete-tag-if-exists tag)]
    [_ (generate-index tag paths)]))

;; we might consider "resetting" the map (by building the map afresh) after
;; an exception occurs

(delete-all-tags)
(for ([tag (apply set-union (map list->set (cons '() (hash-values path-map))))])
  (process-tag tag))
