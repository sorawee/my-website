#lang racket/base

(provide (all-defined-out))

(require racket/match
         racket/contract
         racket/function
         racket/path
         racket/file
         pollen/core
         pollen/setup
         "utils/path.rkt"
         "utils/date.rkt")

; Modified from https://github.com/malcolmstill/mstill.io/blob/master/blog/pollen.rkt
(define/contract (post-filename->date path-string) (path-string? . -> . (or/c date*? #f))
  (match (if (path? path-string) (path->string path-string) path-string)
    [(pregexp #px"blog/(\\d\\d\\d\\d)/(\\d\\d)-(\\d\\d)-" (list _ year month day))
     (list->date (map string->number (list year month day)))]
    [_ #f]))

(define/contract (is-draft? path) (path-string? . -> . any/c)
  ;; this should really be a boolean? instead of any/c, but member is stupid
  (member 'DRAFT (select-from-metas 'tags (get-metas path))))

(define/contract (pollen-post? path) (path-string? . -> . boolean?)
  (match (build-path-string path)
    [(pregexp #px"blog/\\d+/.*\\.poly\\.pm$") #t]
    [_ #f]))


(define/contract (all-posts #:draft? [draft? #f])
  (() (#:draft? boolean?) . ->* . (listof path?))
  (define fake-is-draft? (if draft? (λ (_) #f) is-draft?))
  (reverse
   (sort (map rel-path (find-files (conjoin pollen-post? (negate fake-is-draft?))
                                   (build-path (current-project-root) "blog")))
         path<?)))
