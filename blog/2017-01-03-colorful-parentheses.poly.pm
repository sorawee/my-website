#lang pollen

@(define-meta title "Colorful Parentheses!")
@(define-meta tags (racket blog))

I tried to learn @link["https://en.wikipedia.org/wiki/Continuation"]{continuations} yesterday. This led me to a @link["http://community.schemewiki.org/?p=composable-continuations-tutorial"]{tutorial for composable continuations} in schemewiki.org. It explains things nicely, but what really intrigues me is something completely different: the colorful syntax highlight for nested parentheses when hovering cursor over them.

@center{@img["/blog/img/paren-highlight.png" #:width "50%"]}

This post, of course, explains how I managed to get it on my website!

@see-more

@highlight['racket]|{
  (define x (+ 1 (+ 3 (+ 2 4) (+ 9 9))))
}|

First step is straightforward: look at the source code. Here's the result:

@filebox-highlight["http://community.schemewiki.org?composable-continuations-tutorial" 'html]|{
  ...
  <span class="paren">(<a
    href="http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_278"
        class="scheme-documentation">+</a> 1 <span class="paren">(<a
    href="http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_278"
        class="scheme-documentation">+</a> 3 <span class="paren">(<a
    href="http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_idx_278"
        class="scheme-documentation">+</a> 2 4)</span>)</span>)</span>
  ...
}|

for @code{(+ 1 (+ 3 (+ 2 4)))}. Something is going on with that class @code{paren}... Perhaps JavaScript? But I searched for @code{js} in the source code and found nothing...

CSS? I saw @code{<link rel="stylesheet" href="/css/default.css" type="text/css">} at the top of the file...

@filebox-highlight["http://community.schemewiki.org/css/default.css" 'css]|{
...
/* The paren stuff */

/* Top level */
PRE.scheme > SPAN.paren:hover { background-color: #FFCFCF }

/* Paren level 1 */
PRE.scheme > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFCF }

/* Paren level 2 */
PRE.scheme > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFCFFF }

/* Paren level 3 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFFF }

/* Paren level 4 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #FFCFFF }

/* Paren level 5 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #FFFFCF }

/* Paren level 6 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren
> SPAN.paren:hover { background-color: #B4E1EA }

/* Paren level 7 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #BDEAB4 }

/* Paren level 8 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #EAD4B4 }

/* Paren level 9 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #F4D0EC }

/* Paren level 10 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #D0D9F4 }

/* Paren level 11 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren
> SPAN.paren:hover { background-color: #FFCFCF }

/* Paren level 12 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFCF }

/* Paren level 13 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFCFFF }

/* Paren level 14 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFFF }

/* Paren level 15 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #FFCFFF }

/* Paren level 16 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren
> SPAN.paren:hover { background-color: #FFFFCF }

/* Paren level 17 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #BDEAB4 }

/* Paren level 18 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #EAD4B4 }

/* Paren level 19 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #F4D0EC }

/* Paren level 20 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #D0D9F4 }

/* Paren level 21 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren
> SPAN.paren:hover { background-color: #FFCFCF }

/* Paren level 22 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFCF }

/* Paren level 23 */
PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:hover { background-color: #CFFFCF }

PRE.scheme > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren > SPAN.paren
           > SPAN.paren > SPAN.paren > SPAN.paren
> SPAN.paren:before { content: "{{23 levels of indentation?! Yiakes!}}" }

/* extend here if more nestings are needed */
...
}|

Oh yeah...@numbered-note{In fact, what I should do is to generate the CSS file according to the maximum number of depth of all source codes...}

This method requires that the matching parentheses are grouped together in one @code{span}. This would be easy for raw text because in Racket we have @code{read} which would transform the whole code to an S-Expression, grouping matching parentheses nicely. However, the current workflow is to use Pygments on the raw text to get the syntax highlight for keywords. Suppose we were to do a preprocess on the raw text to add some information about matching parentheses, Pygments could highlight the code completely wrong. But if we were to do a postprocess on the output of Pygments, we would get HTML nodes instead, which is not readable by @code{read}.

So we will do the postprocess, and instead of relying on @code{read}, we will parse the data manually. Note, though, that Pygments is not only doing a syntax highlight. It also acts as a lexer. That means left parenthesis and left parenthesis in a quote would be distinguishable, which is pretty nice. Here's an example of an output from Pygments:

@highlight['racket]|{
  '(div ((class "highlight")) (table ((class "sourcetable"))
  (tbody (tr (td ((class "linenos")) (div ((class "linenodiv"))
  (pre " 1\n 2\n 3\n 4\n 5\n..."))) (td ((class "code")) (div ((class "source"))
  (pre (span) (span ((class "kn")) "#lang ") (span ((class "nn")) "racket") "\n\n"
  (span ((class "p")) "(") (span ((class "k")) "define-syntax-rule") " "
  (span ((class "p")) "(") (span ((class "n")) "def") " " (span ((class "n")) "whatever")
  " " (span ((class "k")) "...") (span ((class "p")) ")") " " (span ((class "p")) "(")
  (span ((class "k")) "define") " " (span ((class "n")) "whatever") " "
  (span ((class "k")) "...") (span ((class "p")) "))") "\n" (span ((class "p")) "(")
  (span ((class "k")) "define-syntax-rule") " " ...) ...) ...) ...) ...) ...) ...)
}|

This blob of data contains what we want, but it's not exactly a stream of tokens. To extract that out, we use a straightforward pattern matching@numbered-note{And I thought, isn't there jQuery for X-Expression? Then I found @link["http://cs.brown.edu/~sk/Publications/Papers/Published/kk-sxslt/"]{this paper}... It's not compatible with X-Expression though, and that's why I didn't use it.}:

@highlight['racket]|{
  (match (highlight ...)
    [`(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ,things-in-pre ...)) "\n")))) "\n")
     `(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos))) (td ((class "code")) (div ((class "source")) (pre ((class "scheme")) ,@(parenthesize things-in-pre))) "\n")))) "\n")])
     ; add class scheme to pre to make CSS works
}|

So what we need to do is to write a function @code{parenthesize} to match @code{(span ((class "p")) "(")} with @code{(span ((class "p")) ")")}. Notice however that right now adjacent parentheses are grouped together, like @code{(span ((class "p")) "))")}. Therefore, we need to first normalize them by splitting them to multiple tokens: @code{(span ((class "p")) ")")} and @code{(span ((class "p")) ")")}.

@highlight['racket]|{
  (define (normalize lst)
    (match lst
      [(list `(span ((class "p")) ,str) rst ...)
       (append (map (λ (x) `(span ((class "p")) ,(string x))) (string->list str))
               (normalize rst))]
      [(list fst rst ...) (cons fst (normalize rst))]
      [_ lst]))
}|

After normalization, we now need to do the real work which is to parse and group matching parentheses together. There are obvious and efficient algorithms to do this, but I've heard that Racket's pattern matching is pretty powerful, so let's try that. The idea is to find an innermost matching parentheses and group them one at a time, then repeat until there is no more matching parentheses left.

@highlight['racket]|{
  (define left-paren '(span ((class "p")) "("))
  (define right-paren '(span ((class "p")) ")"))
  (define not-paren? (negate (curryr member (list left-paren right-paren))))
  (define (iter lst)
    (match lst
      [(list prefix-tok ...
             (== left-paren)
             (? not-paren? inside) ...
             (== right-paren)
             suffix-tok ...)
       (iter (append prefix-tok
                     (list `(span [[class "paren"]]
                                  ,left-paren ,@inside ,right-paren))
                     suffix-tok))]
      [_ lst]))
}|

And finally define @code{parenthesize}:

@highlight['racket]|{
  (define parenthesize (compose1 iter normalize))
}|

And @emph{holy cow, it works!} Pretty quickly too. The exact time complexity is unclear since I do not know how the magic of pattern matching works. Note that the matching patterns are not static data, so the compiler would not be able to exploit this much. Thus, we can assume that the compiler will generate a dumb code to try to match things. Finding an innermost matching parentheses definitely takes at least linear time, and we repeat it until there's no more matching parentheses. That is, the algorithm would be at least quadratic. Since the code in my blog would be at most 500 lines long, that's pretty chill.

But let's see how we will improve this and make it linear time. There are in fact several ways to do it. @link["http://justinpombrio.net"]{Justin} particularly likes the stack solution one:

@highlight['racket]|{
  (define left-thing? (curryr member (list left-paren left-bracket)))
  (define right-thing? (curryr member (list right-paren right-bracket)))
  (define-values (parsed-flipped)
    (for/fold ([stack '()]) ([e (normalize lst)])
      (match e
        [(? left-thing? lp) (values (cons lp stack))]
        [(? right-thing? rp)
         (define-values (grouped new-stack) (splitf-at stack (negate left-thing?)))
         (values
           (match new-stack
             [(list (? left-thing? lp) _ ...)
              (match (list lp rp)
                [(list (== left-paren) (== right-paren)) #f]
                [(list (== left-bracket) (== right-bracket)) #f]
                [_ (error 'mismatched-type-paren)])
              (cons `(span [[class "paren"]]
                           ,@(reverse (append (list rp) grouped (list lp))))
                    (rest new-stack))]
             [_ (cons e stack)]))] ; if too many right parentheses
        [_ (values (cons e stack))])))
  (reverse parsed-flipped)
}|

This algorithm also tolerates mismatched parentheses@numbered-note{I haven't tested this functionalities that much because, you know, normally I wouldn't have mismatched parentheses. So... it's possible that this will be buggy when there are mismatched parentheses.} (but not mismatch type), because it's possible that I will just copy and paste only a part of a code from somewhere else, which doesn't have complete matching parentheses.

@highlight['racket]|{
  ; this has too many right parentheses
  (cons 1 2))
  ; this has too many left parentheses
  ((cons 3 4)
}|

And that, my friends, is how you get colorful parentheses. @emj{:)}
