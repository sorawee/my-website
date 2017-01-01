#lang racket

(require pollen/pagetree
         pollen/core
         racket/file
         web-server/templates
         pollen/cache)

(reset-cache)

(define (get-all-tags s)
  (match s
    ['() empty]
    [(cons f r) (append (get-all-tags f) (get-all-tags r))]
    [(? (λ (x) (file-exists? (symbol->string x))))
     (map (λ (x) (list x s)) (or (select-from-metas 'tags s) empty))]
    [_ empty]))

(define tags
  (map (lambda (tag-data) (list (first (first tag-data))
                                (map (λ (f) (second f)) tag-data)))
       (group-by first (get-all-tags (get-pagetree "index.ptree")))))

(for/list ([dir (directory-list "tags")])
  (define real-dir (string-append "tags/" (path->string dir)))
  (when (directory-exists? real-dir) (delete-directory/files real-dir)))

(for/list ([tag tags])
  (define tagname (symbol->string (first tag)))
  (define posts (string-join (map (λ (post) (string-append "'" (symbol->string post)))
                                  (second tag))))
  (define dir (string-append "tags/" tagname))
  (make-directory dir)
  (define f (open-output-file	(string-append dir "/index.html.pm") #:exists 'replace))
  (display (include-template #:command-char #\$ "../tags/tag.template") f)
  (close-output-port f))
