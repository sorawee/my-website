#lang racket/base

(provide make-post get-summary)
(require racket/contract
         racket/match
         racket/list
         racket/string
         racket/date
         racket/function

         pollen/core
         pollen/setup
         pollen/file

         txexpr
         threading

         "tags.rkt"
         "contracts.rkt"

         "post-file-utils.rkt"
         "meta-utils.rkt"
         "utils/path.rkt"
         "utils/date.rkt"

         "../config.rkt")

(define/contract (make-post post
                            #:see-more? [see-more? #f]
                            #:header? [header? #t])
  ((path-string?) (#:see-more? boolean? #:header? boolean?) . ->* . content?)

  (define/contract (make-tag tag) (string? . -> . content?)
    `(a [[href ,(build-path-string path-prefix-lang "tags" tag)]] ,tag))

  (define path (build-path-string (current-project-root) post))
  (printf "Make post with: ~a and ~a\n" post path)
  (define content
    (~> (get-doc path)
        (match _
          [`(@@app ,_ ,doc) doc]
          [_ (error 'make-post "@@app not found during rendering ~a" post)])
        ((if see-more? get-summary values) _)
        get-elements))
  (define post-uri (build-path-string path-prefix-lang (->output-path post)))
  (define/contract (get-see-more) (-> content?)
    `(div ([class "see-more-link"])
          (a ([href ,post-uri])
             (span ([class "smallcaps"]) "[see more]"))))
  (define/contract (get-update-on) (-> list?)
    (define update-on (hash-ref (get-metas path) 'updated #f))
    (cond
      [update-on
       `((p ([class "update"])
            (span ([class "pubdate"]) "Updated on "
                  ,(date->string (list->date update-on)))))]
      [else '()]))
  `(@ ,(if header?
           `(h2 (a ([href ,post-uri])
                   ,(title->string (select-from-metas 'title path))))
           "")
      (p (span ([class "pubdate"]) "Published on "
               ,(date->string (post-filename->date post)))
         " :: "
         (span
          ,@(add-between
             (map make-tag (map symbol->string (select-from-metas 'tags path))) ", ")))
      ,@(get-update-on)
      (div ([class "post-content"]) ,@content)
      ,(if see-more? (get-see-more) "")))

;; Section: Summary

(define/contract (get-summary tx-in) (content? . -> . (or/c #f content?))
  (define tags-skip '(label input))
  (define class-skip '("margin-note"))
  (define found #f)
  (define tx-out
    (let iter ([tx tx-in])
      (match tx
        [(txexpr t attrs elems)
         (cond
           [(member t see-more)
            (set! found #t)
            #f]
           [(or (member t tags-skip)
                (ormap (curryr member class-skip)
                       (string-split (second (or (assoc 'class attrs) '("" "")))
                                     " "))) ""]
           [else
            (let-values
                ([(elems)
                  (for/fold ([acc-elements empty]) ([ele elems]) #:break found
                    (define item (iter ele))
                    (values (if (or (not found) item)
                                (cons item acc-elements)
                                acc-elements)))])
              (txexpr t attrs (reverse elems)))])]
        [_ tx])))
  (and found tx-out))

(module+ test
  (require rackunit)
  (check-equal? (get-summary '(root (p "0" "1") (p "2" (see-more) "3") (p "4" "5")))
                '(root (p "0" "1") (p "2"))))

;; End Section
