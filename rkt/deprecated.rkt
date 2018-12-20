
(define (pdfable? file-path)
  (string-contains? file-path ".poly"))

(define (pdfname page) (string-replace (path->string (file-name-from-path page))
                                       "poly.pm" "pdf"))


(define (proof #:wrong [wrong #f] . xs) `(div [[class "proof"]] (span [[class "subproof"]] (i "Proof. ") ,@xs) ))
(define (lemma . xs) `(div [[class "lemma"]] (b "Lemma ") ,@xs))

(define (pre #:options options . xs)
  (define hash #hasheq((allowed-math . "tex2jax_process")))
  (define (get-class options)
    (string-join (filter-map (λ (option) (hash-ref hash option #f)) options)))
  `(pre [[class ,(get-class options)]] ,@xs))


(define/contract (argumentlist . items) (item? ... . -> . xexpr/c)
  (define-values (premises conclusion) (split-at items (sub1 (length items))))
  (match-define `((li ,xs ...))  conclusion)
  `(div [[class "argument"]] (ol [[style "margin-bottom: 0;"]] ,@premises) (ul [[class "no-bullet"]] (li (hr [[class "logic-separator"]])) (li [[class "logic-conclusion"]] ,@xs))))

(define (LaTeX) `(span [[class "latex"]]
                       "L"
                       (span [[class "latex-sup"]] "a")
                       "T"
                       (span [[class "latex-sub"]] "e")
                       "X"))

(define (phantom . xs) `(div [[style "display: none"]] ,@xs))

(define (tt . xs) `(tt ,@xs))


(define (outdated . xs)
  `(div [[class "outdated"]]
        (div (u (b "Outdated:")))
        (div ,@xs)))

(define (figure path #:width [width "40%"] #:caption [caption '()])
  `(center ,(img path #:width width) (br) ,@caption))

(define (task . xs) `(u ,@xs))


(define (notice . xs) `(i ,@xs))
(define (migration-notice) (notice "This post is migrated from my old blog."))

(define (eng . xs) (! (append (list "(ภาษาอังกฤษ: ") xs (list ")"))))


(define (^ . xs) `(sup ,@xs))


(define (canvas id)
  `(table (tr (td (canvas [[id ,id] [width "560"] [height "200"]])))))


(define (folded title . xs)
  (define refid (uuid-generate))
  `(@ (label [[for ,refid] [class "fold-toggle"]] ,title)
      (input [[type "checkbox"] [id ,refid] [class "fold-toggle"]])
      (span [[class "folded-content"]] ,@xs)))

(define (spoiler . xs)
  (apply folded (cons "[spoiler ⊕]" xs)))
