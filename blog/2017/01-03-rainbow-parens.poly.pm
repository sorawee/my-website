#lang pollen

@(define-meta title "Rainbow in Pygments")
@(define-meta tags (racket blog pygments))
@(define-meta updated (2018 12 6))

I tried to learn @link["https://en.wikipedia.org/wiki/Continuation"]{continuations} yesterday. This led me to a @link["http://community.schemewiki.org/?p=composable-continuations-tutorial"]{tutorial on composable continuations} in schemewiki.org. It explains things nicely, but what really intrigues me is something completely different: @link["https://www.emacswiki.org/emacs/RainbowDelimiters"]{rainbow parentheses} on mouse hovering.

@img["img/01-paren-highlight.png" #:width "60%"]

This post, of course, explains how I managed to get it on my website!

@highlight['racket]|{
  (define x (+ 1 (+ 3 (+ 2 4) (+ 9 9))))
}|

@see-more

@section{Hacking schemewiki.org}

Since I do not know how to implement this feature, the first step is obvious: take a look at the source code in schemewiki.org. For @code{(+ 1 (+ 3 (+ 2 4)))}, here's the result:

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

Something is going on with the class @code{paren}... Perhaps they use JavaScript to add the rainbow? But I searched for @code{js} in the source code and found nothing...

If it's not JavaScript, then perhaps CSS? I saw @code{<link rel="stylesheet" href="/css/default.css" type="text/css">} at the top of the file...

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

Mystery solved!

@section{Integration with Pygments}

This rainbow trick requires that matching parentheses are grouped together in a @code{span}. The obvious method to achieve this is to use the @link["https://docs.racket-lang.org/reference/Reading.html#%28def._%28%28quote._~23~25kernel%29._read%29%29"]{@code{read}} function to transform string into an @link["https://en.wikipedia.org/wiki/S-expression"]{S-Expression}, thereby grouping matching parentheses together. However, one complication is that I also use @link["http://pygments.org/"]{Pygments} for syntax highlight. It's not clear how to integrate all of these together. Let's explore a couple of options:

@itemlist[
  @item{If we group parentheses first by using @code{read}, the output would be an S-expression. Pygments, which expects a string as an input, won't be able to function. That means we need to manually highlight syntax... which seems really difficult.}
  @item{If we use Pygments first, the output would be an @link["https://docs.racket-lang.org/xml/index.html#%28def._%28%28lib._xml%2Fprivate%2Fxexpr-core..rkt%29._xexpr~3f%29%29"]{X-expression} representing an HTML DOM tree. @code{read}, which expects a string as an input, won't be able to function. That means we need to manually extract and parse code from the tree. This is not trivial at all. For instance, it's not clear how to distinguish between an actual left parenthesis and a left parenthesis as a string.}
]

However, one cool feature of Pygments is that it does not only do a syntax highlight but also acts as a lexer. Here's an output example from Pygments:

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

Notice that all parentheses are tagged with the class @code{p} properly, so the second approach looks really promising. But while this blob of data contains what we want, it's not exactly a stream of tokens. To extract that out, we use a straightforward pattern matching@margin-note{And I thought, isn't there jQuery for X-Expression? Then I found @link["http://cs.brown.edu/~sk/Publications/Papers/Published/kk-sxslt/"]{@emph{SXSLT: Manipulation Language for XML}}... It's not compatible with X-Expression though, so I didn't use it.}:

@highlight['racket]|{
  (match (highlight my-code-str)
    [`(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr
    (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos)))
    (td ((class "code")) (div ((class "source"))
      (pre ,things-in-pre ...)) "\n")))) "\n")
     `(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr
     (td ((class "linenos")) (div ((class "linenodiv")) (pre ,linenos)))
     (td ((class "code")) (div ((class "source"))
       (pre ((class "scheme")) ,@(parenthesize things-in-pre))) "\n")))) "\n")])
     ; add class scheme to pre to make CSS works
}|

What we need to do next is to write a function @code{parenthesize} to match @code{(span ((class "p")) "(")} with @code{(span ((class "p")) ")")}. Note however that right now adjacent parentheses are grouped together, like @code{(span ((class "p")) "))")}. Therefore, we need to first normalize them by splitting them to multiple tokens: @code{(span ((class "p")) ")")} and @code{(span ((class "p")) ")")}.

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
                     (list `(span ([class "paren"])
                                  ,left-paren ,@inside ,right-paren))
                     suffix-tok))]
      [_ lst]))
}|

And finally define @code{parenthesize}:

@highlight['racket]|{
  (define parenthesize (compose1 iter normalize))
}|

And @emph{holy cow, it works!} Pretty quickly, too. The exact time complexity is unclear since I do not exactly know how Racket's magical pattern matching works. Note that the matching patterns are not static data, so the compiler would not be able to exploit this much. We can assume that the compiler will generate a dumb code to try to match things. Finding an innermost matching parentheses definitely takes at least linear time, and we repeat it until there's no more matching parentheses. That is, the algorithm would be at least quadratic. Since the code in my blog would be at most 500 lines long, that's pretty chill.

But we can improve this and make it linear time. There are several ways to do it. I came up with a stack-based solution, and @link["http://justinpombrio.net"]{Justin} seems to like it:

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
              (cons `(span ([class "paren"])
                           ,@(reverse (append (list rp) grouped (list lp))))
                    (rest new-stack))]
             [_ (cons e stack)]))] ; if too many right parentheses
        [_ (values (cons e stack))])))
  (reverse parsed-flipped)
}|

This algorithm also tolerates mismatched parentheses (but not mismatch type). This is great because it's possible that I will just copy and paste only a part of a code from somewhere else, which doesn't have complete matching parentheses.

@highlight['racket]|{
  ; this has too many right parentheses
  (cons 1 2))
  ; this has too many left parentheses
  ((cons 3 4)
}|

And this is how I get rainbow parentheses on my site. @emj{:)}
