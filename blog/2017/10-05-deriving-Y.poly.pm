#lang pollen

@(define-meta title "Deriving the Y Combinator")
@(define-meta tags (programming-languages y-combinator))
@(define-meta updated (2018 12 9))

The Y combinator is a somewhat magical aspect of the untyped lambda calculus. Many people presented explanations of this magic, but I found them somewhat unsatisfactory. Some explanations start by showing the combinator in the beginning and then simply demonstrate that the given combinator is correct. Some other explanations contain lines like "Why don't we try passing it to itself? And look, this turns out to be exactly what we want!" These explanations don't give an intuition for how one could @emph{constructively} obtain the combinator. @emph{What on earth makes you think that passing it to itself is a good idea?} As such, this post, intended for those who know the Y combinator already but do not satisfy with the existing explanations, attempts to explain how one could constructively derive the Y combinator.

@see-more

@section{Introduction}

In @link["https://en.wikipedia.org/wiki/Lambda_calculus"]{lambda calculus}, there's no primitive support to define recursive functions. @emph{Can we define recursive functions in lambda calculus regardless? And how?}

While our question focuses on lambda calculus, to make it easier to follow, we will use an actual programming language. Here, I choose to use @link["http://racket-lang.org"]{Racket}@margin-note{The choice of Racket is that:
@itemlist[
  @item{It is not statically typed, so we can write the combinator which would have been prohibited had we use a language with a standard type system like @link["https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system"]{Hindley–Milner}.}
  @item{Its scoping rule is simple and close to lambda calculus. Unlike, say, JavaScript and Python, there's no variable hoisting, so we won't encounter a problem where a simple, innocent code @code{const f = () => f();} unintentionally allows recursion.}
]}, but other programming languages might work as well.

As mentioned above, this post is intended for those who know basic programming language theory and the Y combinator already. However, I will try to provide links as many as possible to accommodate non-target audience.

@section{Enabling recursion}

In Racket, we can write code to compute @${3! + 4!} easily@margin-note{This post will mostly use @code{#lang racket}, so I will elide @link["https://docs.racket-lang.org/guide/more-hash-lang.html"]{the hash lang line} from now on unless I want to highlight it.}.

@highlight['racket]{
#lang racket

(letrec ([fact (λ (n)
                 (case n
                   [(0) 1]
                   [else (* n (fact (- n 1)))]))])
  (+ (fact 3) (fact 4)))
; => 30
}

Defining this recursive @code{fact} function is possible because @link["https://docs.racket-lang.org/guide/let.html#%28part._.Recursive_.Binding__letrec%29"]{@code{letrec}}@margin-note{Read @code{(letrec ([@mvar{x} @mvar{v}]) @mvar{e})} as "let @code{@mvar{x}} be a recursive function @code{@mvar{v}} and then return @code{@mvar{e}}"} allows us to refer to @code{fact} inside the lambda function that @emph{will} be @code{fact} itself.

Of course, lambda calculus has no @code{letrec}. We might ask how Racket implements @code{letrec}, and the short answer is that it does so by using mutation, which lambda calculus doesn't have either. To simulate lambda calculus, we will write the factorial function in Racket without using @code{letrec} and mutation. Here's our first straightforward attempt:

@highlight['racket]{
(let ([fact (λ (n)
              (case n
                [(0) 1]
                [else (* n (@mark["focus"]{fact} (- n 1)))]))])
  (+ (fact 3) (fact 4)))
;; => fact: unbound identifier in: fact
}

We simply change @code{letrec} to @link["https://docs.racket-lang.org/guide/let.html#%28part._.Parallel_.Binding__let%29"]{@code{let}}@margin-note{Read @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} as "let @code{@mvar{x}} be @code{@mvar{v}} and then return @code{@mvar{e}}"}. While @code{let} and @code{letrec} are very similar, @code{let} doesn't have the ability that allows us to refer to @code{fact} inside that lambda function. In fact, @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} doesn't add any expressive power to lambda calculus at all because its semantics is completely equivalent to @code{((λ (@mvar{x}) @mvar{e}) @mvar{v})}, a lambda term. Some programming languages simply treats @code{let} as a @link["https://en.wikipedia.org/wiki/Syntactic_sugar"]{syntactic sugar}, and desugar @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} to @code{((λ (@mvar{x}) @mvar{e}) @mvar{v})}.

As expected, our straightforward attempt doesn't work because the highlighted @code{fact} is unbound. How can we fix it? Let's take a look at another example:

@highlight['racket]{
(let ([f (λ (n) (+ n @mark["focus"]{c}))])
  (let ([c 1])
    (f 99)))
;; => c: unbound identifier in: c
}

The above code has a similar problem. @code{c} is unbound inside the body of the lambda. One way to fix the problem is to explicitly pass @code{c} to the lambda function, so that @code{c} is bound:

@highlight['racket]{
(let [[f (λ (@mark["add"]{c} n) (+ n c))]]
  (let [[c 1]]
    (f @mark["add"]{c} 99)))
;; => 100
}

Our solution to the unbound @code{fact} problem is going to be the same. While @code{fact} is unbound inside the lambda function, it is bound in the body of @code{let}, so we can pass @code{fact} explicitly to the lambda function!

@highlight['racket]{
(let ([fact (λ (@mark["add"]{fact} n)
              (case n
                [(0) 1]
                [else (* n @mark["focus"]{(fact (- n 1))})]))])
  (+ (fact @mark["add"]{fact} 3) (fact @mark["add"]{fact} 4)))
;; => fact: arity mismatch;
;; the expected number of arguments does not match the given number
;;  expected: 2
;;  given: 1
;;  arguments...:
}

There's still a problem: @code{fact} is now a function of arity 2, so the highlighted function application would result in an arity mismatch error. What does @code{fact} need as the first argument? The answer is @code{fact}! So we can fix the code accordingly:

@highlight['racket]{
(let ([fact (λ (fact n)
              (case n
                [(0) 1]
                [else (* n (fact @mark["add"]{fact} (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
;; => 30
}

With this trick, we can write any recursive function in a language without @code{letrec} by the following method:

@numberlist[
  @item{Initially write the recursive function @${f} with @code{letrec}.}
  @item{Change @code{letrec} to the @code{let}.}
  @item{At every callsite of @${f}, prepend the arguments with @${f}@|apos|s name}
  @item{Prepend the parameters of @${f} with @${f}@|apos|s name}
]

This is simply a syntax transformation which could be done mechanically.

@section{Omega}

Let's digress a little bit here. What will happen if we perform the syntax transformation to this function which loops forever?

@highlight['racket]{
(letrec ([diverge (λ () (diverge))])
  (diverge))
}

is transformed to:

@highlight['racket]{
(let ([diverge (λ (diverge) (diverge diverge))])
  (diverge diverge))
}

To obtain a lambda term, we simply desugar @code{let} as described above:

@highlight['racket]|{
  ((λ (diverge) (diverge diverge)) (λ (diverge) (diverge diverge)))
}|

Recall that this is known as the @${\Omega}-combinator, a classic example of a lambda term whose evaluation doesn't terminate.

@section{Deriving a fake Y combinator}

While we now know how to write recursive functions in lambda calculus, the transformed function doesn't look like the original function that we would write in the @code{letrec} version. It would be nice if there is a term @code{@mvar{make-recursion-possible}} such that:

@highlight['racket]{
(@mvar{make-recursion-possible}
  (λ (n)
    (case n
      [(0) 1]
      [else (* n (@mark["focus"]{fact} (- n 1)))])))
}

produces the actual factorial function. However, this clearly won't work, because the highlighted identifiers are going to be unbound. To fix this, we simply make it bound by wrapping a lambda function around the body.

@highlight['racket]{
(@mvar{make-recursion-possible}
  (@mark["add"]{λ (fact)}
    (λ (n)
      (case n
        [(0) 1]
        [else (* n (fact (- n 1)))]))))
}

We will call the lambda function that is the argument of @code{@mvar{make-recursion-possible}} a @emph{recursion maker}. And what we want is to find @code{@mvar{make-recursion-possible}} so that for any @code{@mvar{recursion-maker}}, @code{(@mvar{make-recursion-possible} @mvar{recursion-maker})} produces the actual recursive function.

How should we approach this? We don't know what @code{@mvar{make-recursion-possible}} looks like. But for the factorial function, we do know what @code{@mvar{fact-maker}} looks like. The strategy, then, is to extract the @code{@mvar{fact-maker}}:

@highlight['racket]{
(λ (fact)
  (λ (n)
    (case n
      [(0) 1]
      [else (* n (fact (- n 1)))])))
}

from our current code:

@highlight['racket]{
(let ([fact (λ (fact n)
              (case n
                [(0) 1]
                [else (* n (fact fact (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
}

Hopefully, what's left is going to be @code{@mvar{make-recursion-possible}}

To extract @code{@mvar{fact-maker}}, as a first step, we can avoid prepending @code{fact} at the callsites of @code{fact} inside the lambda function. Simply transform:

@highlight['racket]{
(let ([fact (λ (fact n)
              (case n
                [(0) 1]
                [else (* n (fact fact (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
}

to

@highlight['racket]{
(let ([fact (λ (fact n)
              (let ([fact (λ (n) (fact fact n))])
                @mark["focus"]{
(case n
                  [(0) 1]
                  [else (* n (fact (- n 1)))])
                }))])
  (+ (fact fact 3) (fact fact 4)))
}

which shadows @code{fact} that consumes two arguments with @code{fact} that consumes one argument while maintaining the semantics. The body of the function is now similar to @code{@mvar{fact-maker}}, especially for the highlighted part. What can we do next? We see that @code{fact} and @code{n} are both the formal parameters in the transformed code, whereas in @code{@mvar{fact-maker}} they are separated. Straightforwardly, we can use @link["https://en.wikipedia.org/wiki/Currying"]{currying} to separate the two apart. We of course need to change how we call the function as well:

@highlight['racket]{
(let ([fact (λ (fact)
              (λ (n)
                (let ([fact (λ (n) ((fact fact) n))])
                  (case n
                    [(0) 1]
                    [else (* n (fact (- n 1)))]))))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}

The @code{(let ([fact (λ (n) ((fact fact) n))]) ...)} doesn't depend on the parameter @code{n}, so we can lift it up without changing the semantics:

@highlight['racket]{
(let ([fact (λ (fact)
              (let ([fact (λ (n) ((fact fact) n))])
                @mark["focus"]{
(λ (n)
                  (case n
                    [(0) 1]
                    [else (* n (fact (- n 1)))]))
                }))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}

Now our highlighted code is very similar to the @code{@mvar{fact-maker}}! What we want to do next is to somehow convert @code{(let ([fact ...]) body...)} into @code{(λ (fact) body...)} and move @code{(λ (n) ((fact fact) n))} somewhere else. That's simply desugaring @code{let}!

@highlight['racket]{
(let ([fact (λ (fact)
              (@mark["focus"]{
(λ (fact)
                 (λ (n)
                   (case n
                     [(0) 1]
                     [else (* n (fact (- n 1)))])))
              }
               (λ (n) ((fact fact) n))))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}

and we finally have the @code{@mvar{fact-maker}}!

Our next mission is to obtain the @code{@mvar{make-recursion-possible}}. To make things tidy, let's first abstract @code{@mvar{fact-maker}} out as a variable named @code{fact-maker}:

@highlight['racket]{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (case n
                        [(0) 1]
                        [else (* n (fact (- n 1)))])))])
  (let ([fact (λ (fact) (fact-maker (λ (n) ((fact fact) n))))])
    (+ ((fact fact) 3) ((fact fact) 4))))
}

Recall that @code{(@mvar{make-recursion-possible} fact-maker)} should produce the actual factorial function which consumes only one number as an argument. Right now we still have @code{((fact fact) 3)} instead of @code{(fact 3)}. We can fix this using shadowing like what we did earlier:

@highlight['racket]{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (case n
                        [(0) 1]
                        [else (* n (fact (- n 1)))])))])
  (let ([fact (λ (fact) (fact-maker (λ (n) ((fact fact) n))))])
    (let ([fact (fact fact)])
      (+ (fact 3) (fact 4)))))
}

Two nested @code{let} can be fused together in a straightforward way:

@highlight['racket]{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (case n
                        [(0) 1]
                        [else (* n (fact (- n 1)))])))])
  (let ([fact ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
               (λ (fact) (fact-maker (λ (n) ((fact fact) n)))))])
    (+ (fact 3) (fact 4))))
}

As a final step, @code{@mvar{make-possible-recursion}} should consume @code{fact-maker} as an argument, so we can abstract @code{fact-maker} out:

@highlight['racket]{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (case n
                        [(0) 1]
                        [else (* n (fact (- n 1)))])))])
  (let ([fact ((λ (fact-maker)
                 ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
                  (λ (fact) (fact-maker (λ (n) ((fact fact) n))))))
               fact-maker)])
    (+ (fact 3) (fact 4))))
}

And we are done! We have derived the @code{@mvar{make-recursion-possible}} as follows:

@highlight['racket]{
(λ (fact-maker)
  ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
   (λ (fact) (fact-maker (λ (n) ((fact fact) n))))))
}

Notice that there's nothing inherent about the factorial function here. If we use the Fibonacci function as a starting point, we would end up with a term that is @link["https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence"]{@${\alpha}-equivalent} to this @code{@mvar{make-recursion-possible}} as well. We will highlight this fact by @${\alpha}-convert variable names to remove all references to the factorial function:

@highlight['racket]{
(λ (f)
  ((λ (x) (f (λ (v) ((x x) v))))
   (λ (x) (f (λ (v) ((x x) v))))))
}

This @code{@mvar{make-recursion-possible}} is formally known as the @emph{Z combinator}, and we will subsequently call it @${Z}. It is one of the well-known @emph{fixed-point combinator}s. The word @link["https://wiki.haskell.org/Combinator"]{@emph{combinator}} means that there's no free variable in @${Z}. The word @link["https://en.wikipedia.org/wiki/Fixed_point_(mathematics)"]{@emph{fixed-point}} means that @${Z(f)} is a solution to the equation @${x = f(x)} for any function @${f}. That is, @${Z(f) = f(Z(f))}. While we can see evidently that @${Z} is a combinator, it's not clear that @${Z(f) = f(Z(f))} is true or how this is important. This topic will be further investigated in the next sections.

Recall that the title of this section is @emph{Deriving a fake Y combinator}. It's fake because we actually derived the Z combinator, not the Y combinator. However, they are essentially the same. The difference of the two combinators will also be covered in the next sections.

Here's our final code:

@highlight['racket]{
(let ([Z (λ (f)
           ((λ (x) (f (λ (v) ((x x) v))))
            (λ (x) (f (λ (v) ((x x) v))))))])
  (let ([fact-maker (λ (fact)
                      (λ (n)
                        (case n
                          [(0) 1]
                          [else (* n (fact (- n 1)))])))])
    (let ([fact (Z fact-maker)])
      (+ (fact 3) (fact 4)))))
;; => 30
}

@section{Fixed point}

The goal of this section is to see why @${Z(f) = f(Z(f))} and how it is important. First, we will take a look at recursion makers from another perspective. In the previous section, recursion makers arise naturally, but we only know that @code{(Z @mvar{recursion-maker})} results in a recursive function. What exactly are their role and properties?

Let's take a look at the fact maker as a concrete example:

@highlight['racket]{
(λ (fact)
  (λ (n)
    (case n
      [(0) 1]
      [else (* n (fact (- n 1)))])))
}

What can we pass into @${\text{fact-maker}}? It obviously consumes the factorial function! But provided that we don't have the real factorial function already, the best we can do is to provide any other value instead. Say, we pass in a non-sensible value @${42}. This results in:

@highlight['racket]{
(λ (n)
  (case n
    [(0) 1]
    [else (* n (@mark["focus"]{42} (- n 1)))]))
}

What we have here is the factorial function that only works on @${n = 0}. For @${n > 0}, the highlighted @${42} will be used as a function value which results in an error. We will call this function @${\text{fact}_0}.

What else can we pass into @${\text{fact-maker}}? We just obtained @${\text{fact}_0 = \text{fact-maker}(42)}, so we can use it. This results in:

@highlight['racket]{
(λ (n)
  (case n
    [(0) 1]
    [else (* n (fact0 (- n 1)))]))
}

Again, what we have here is the factorial function that works on @${n \le 1}. Let's call it @${\text{fact}_1}. Next, we can pass @${\text{fact}_1 = \text{fact-maker}(\text{fact-maker}(42))} into @${\text{fact-maker}} to get @${\text{fact}_2}. We can clearly see the trend here. @${\text{fact-maker}^k(42) = \text{fact}_{k-1}}, the factorial function that works on @${n \le k - 1}.

In general, any recursion maker @${f} propels the computation one more level deeper.

To get the actual factorial function, which should work for any natural number @${n}, we evidently need an infinitely high tower of @${\text{fact-maker}}. That is, @${\text{fact} = \text{fact}_\infty = \text{fact-maker}(\text{fact-maker}(\text{fact-maker}(\dots)))}.

Note that:

@$${
\begin{align*}
\text{fact} &= \text{fact-maker}(\text{fact-maker}(\dots))\\
\text{fact-maker}(\text{fact}) &= \text{fact-maker}(\text{fact-maker}(\text{fact-maker}(\dots))) & \text{applying fact-maker}\\
 &= \text{fact}\\
\end{align*}
}

which agrees with our definition of @${\text{fact-maker}}: applying @${\text{fact}} to @${\text{fact-maker}} produces the actual @${\text{fact}} function.

@highlight['racket]{
(λ (fact)
  (λ (n)
    (case n
      [(0) 1]
      [else (* n (fact (- n 1)))])))
}

Recall that the Z combinator makes the recursion possible: @${\text{fact} = Z(\text{fact-maker})}. Substituting this in the above equation, we obtain that @${\text{fact-maker}(Z(\text{fact-maker})) = Z(\text{fact-maker})}.

In general, for any recursion maker @${f}, @${Z(f) = f(Z(f))}. The importance of this identity is that @${Z(f) = f(Z(f)) = f(f(Z(f))) = f(f(f(Z(f)))) = \dots} which creates an infinite tower of the recursion maker @${f}, making recursion possible.

@subsection{Yet another proof}

We can also show that @${Z(f) = f(Z(f))} directly using the definition of @${Z}:

@$${
\begin{align*}
Z &= \lam{f}{(\lam{x}{f (\lam{v}{(x x) v})})(\lam{x}{f (\lam{v}{(x x) v})})} & \\
Z(f) &= (\lam{\color{red}{x}}{f (\lam{v}{(\color{red}{x x}) v})})(\lam{x}{f (\lam{v}{(x x) v})}) & \\
&= f (\lam{\color{blue}{v}}{((\lam{x}{f (\lam{v}{(x x) v})}) (\lam{x}{f (\lam{v}{(x x) v})})) \color{blue}{v}}) & \beta \text{-reduction}\\
&= f ((\lam{x}{f (\lam{v}{(x x) v})}) (\lam{x}{f (\lam{v}{(x x) v})})) & \eta \text{-conversion}\\
&= f (Z(f)) & \\
\end{align*}
}

@section{The Y combinator}

In the previous sections, we derived the Z combinator:

@highlight['racket]{
(λ (f)
  ((λ (x) (f (λ (v) ((x x) v))))
   (λ (x) (f (λ (v) ((x x) v))))))
}

Using @link["https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B7-conversion"]{@${\eta}-conversion}, we obtain the Y combinator:

@highlight['racket]{
(λ (f)
  ((λ (x) (f (x x)))
   (λ (x) (f (x x)))))
}

The @${\eta}-conversion intuitively should preserve functional equivalence. However, this is not totally true because in some @link["https://en.wikipedia.org/wiki/Evaluation_strategy"]{@emph{evaluation strategies}}@margin-note{I personally think that this Wikipedia page is poorly written (as of December 2018), so I will try to summarize the page in this section.}, @${\eta}-conversion could add or remove delayed computation, potentially changing the functional equivalence.

An evaluation strategy describes @emph{what reducible term} (redex) should be @link["https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B2-reduction"]{@${\beta}-reduced}. For example, consider @code{(double (double 1))} where @code{double} is defined as @code{(λ (x) (+ x x))}. There are several strategies to evaluate this expression. The call-by-value strategy always reduces the leftmost-innermost redex (outside of lambda functions):

@highlight['racket]{
(double @mark["focus"]{(double 1)}) ==> (double @mark["add"]{(+ 1 1)})
(double @mark["focus"]{(+ 1 1)})    ==> (double @mark["add"]{2})
@mark["focus"]{(double 2)}          ==> @mark["add"]{(+ 2 2)}
@mark["focus"]{(+ 2 2)}             ==> @mark["add"]{4}
}

The call-by-name strategy, in contrast, always reduces the leftmost-outermost redex (outside of lambda functions):

@highlight['racket]{
@mark["focus"]{(double (double 1))}       ==> @mark["add"]{(+ (double 1) (double 1))}
(+ @mark["focus"]{(double 1)} (double 1)) ==> (+ @mark["add"]{(+ 1 1)} (double 1))
(+ @mark["focus"]{(+ 1 1)} (double 1))    ==> (+ @mark["add"]{2} (double 1))
(+ 2 @mark["focus"]{(double 1)})          ==> (+ 2 @mark["add"]{(+ 1 1)})
(+ 2 @mark["focus"]{(+ 1 1)})             ==> (+ 2 @mark["add"]{2})
@mark["focus"]{(+ 2 2)}                   ==> @mark["add"]{4}
}

The above example seems to show that no matter what strategy we use, the answer will always be the same (although one strategy might be more efficient than the other). Is this intuition correct, and if so, how is it possible for the Y combinator to be functionally equivalent to the Z combinator in only some evaluation strategies?

The answer is given by the @link["https://en.wikipedia.org/wiki/Church%E2%80%93Rosser_theorem"]{Church-Rosser theorem} which states that given a term @${t}, @emph{if two strategies successfully evaluate @${t} to values}, then the two values from both strategies are equal. The theorem shows that our intuition is only partially correct because it doesn't give any guarantee when a strategy doesn't successfully evaluate a term to a value. This is exactly the reason why the Y combinator is not functionally equivalent to the Z combinator in some strategies like call-by-value, as we will see below.

As it turns out, while the call-by-value strategy is generally more efficient (requires fewer reduction steps), it is potentially unsafe since it is possible that the evaluation will result in an infinite loop when reducing some terms that could be successfully reduced to a value had we use another strategy instead. On the other hand, the call-by-name strategy is generally less effecient but safe: if a term could be reduced to a value, it will.

A concrete example is @code{(Y make-fact)}. Using the call-by-value strategy results in an infinite loop:

@highlight['racket]{
1.  (@mark["focus"]{Y} make-fact)
==> (@mark["add"]{(λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x)))))} make-fact)

2.  ((λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))) @mark["focus"]{make-fact})
==> ((λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))) @mark["add"]{(λ (fact) ...)})

3.  @mark["focus"]{((λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))) (λ (fact) ...))}
==> @mark["add"]{((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x))))}

4.  @mark["focus"]{((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x))))}
==> @mark["add"]{
((λ (fact) ...)
     ((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x)))))
    }

5.  ((λ (fact) ...)
     @mark["focus"]{((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x))))})
==> ((λ (fact) ...)
     @mark["add"]{
((λ (fact) ...)
      ((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x)))))
     })

6.  ((λ (fact) ...)
     ((λ (fact) ...)
      @mark["focus"]{((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x))))}))
==> ((λ (fact) ...)
     ((λ (fact) ...)
      @mark["add"]{
((λ (fact) ...)
       ((λ (x) ((λ (fact) ...) (x x))) (λ (x) ((λ (fact) ...) (x x)))))
      }))

...
}

In contrast, using the call-by-name strategy results in the factorial function:

@highlight['racket]{
1.  (@mark["focus"]{Y} make-fact)
==> (@mark["add"]{(λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x)))))} make-fact)

2.  @mark["focus"]{((λ (f) ((λ (x) (f (x x))) (λ (x) (f (x x))))) make-fact)}
==> @mark["add"]{((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x))))}

3.  @mark["focus"]{((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x))))}
==> @mark["add"]{(make-fact ((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x)))))}

4.  (@mark["focus"]{make-fact} ((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x)))))
==> (@mark["add"]{(λ (fact) ...)} ((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x)))))

5.  @mark["focus"]{((λ (fact) ...) ((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x)))))}
==> @mark["add"]{
(λ (n)
      (case
        [(0) 1]
        [1 (* n (((λ (x) (make-fact (x x))) (λ (x) (make-fact (x x))))
                 (- n 1)))]))
    }
}

Most programming languages, including the standard Racket, uses the call-by-value because of its efficiency. Hence, we can't really run the Y combinator in the standard Racket. One cool feature of Racket however is its ability to create and use a new programming language via @link["https://docs.racket-lang.org/guide/more-hash-lang.html"]{the hash lang line}. In fact, the creators of Racket even call Racket @link["https://cacm.acm.org/magazines/2018/3/225475-a-programmable-programming-language/fulltext"]{A Programmable Programming Language}. It is relatively very easy to create a call-by-name language with Racket. Even better, someone did that already and we can just use it!

@highlight['racket]{
#lang lazy

(let ([Y (λ (f)
           ((λ (x) (f (x x)))
            (λ (x) (f (x x)))))])
  (let ([fact-maker (λ (fact)
                      (λ (n)
                        (case n
                          [(0) 1]
                          [else (* n (fact (- n 1)))])))])
    (let ([fact (Y fact-maker)])
      (! (+ (fact 3) (fact 4)))))) ;; we need to ! to force value for printing
;; => 30
}

It appears that call-by-value and call-by-name are historically known as @emph{applicative-order} and @emph{normal-order} respectively@margin-note{@link["http://www.cs.cornell.edu/courses/cs6110/2014sp/Lectures/lec04.pdf"]{Some lectures} state that they are in fact different because applicative-order and normal-order can perform reduction even inside lambda functions.}. For this reason, the Z combinator is also known as the applicative-order Y combinator.

@section{Y and me}

I learned the Y combinator two years ago from the @link["https://cs.brown.edu/courses/cs173/"]{PL class} taught by @link["https://cs.brown.edu/~sk/"]{Shriram Krishnamurthi}. One great explanation (which Shriram pointed us to) is @link["http://felleisen.org/matthias/"]{Matthias Felleisen}@|apos|s @link["https://xivilization.net/~marek/binaries/Y.pdf"]{@italic{A Lecture of the Why of Y}}, which similarly attempts to derive the Y combinator. However, the explanation that corresponds to the "Enabling recursion" section goes in a much slower pace and implicitly contains some elements from the "Fixed point" section. The strength of this approach in my opinion is that it crucially uses the fixed-point identity @${Y(f) = f(Y(f))} to justify the "self-application trick" in the derivation right away, so it motivates really well why @${Y(f) = f(Y(f))} is important. The approach that I use, on the other hand, simply focuses on where things are bound and how do we direct values to appropriate places, which results in a simpler explanation in my opinion. I also make use of @code{let} a lot, which I think helps a lot with readability, with the weakness being that I also need to talk about desugaring.

I didn't discover this approach myself. The core insight of this approach is from a student's homework that I graded in the @link["https://cs.brown.edu/courses/cs173/2017/"]{current iteration} of the PL class. A part of the homework asks students to write random programs in a language that is very similar to Racket without @code{letrec} and mutation. I didn't expect that students will be able to write a really meaningful program unless they know a fixed-point combinator already, ... or so I thought. It turns out that one student wrote a program to calculate 1 + 2 + ... + n for arbitrary n without knowing a fixed-point combinator! Here's the program in Racket:

@highlight['racket]{
(let ([triangle (λ (triangle n)
                  (case n
                    [(0) 0]
                    [else (+ n (triangle triangle (- n 1)))]))])
  (triangle triangle 10))
;; => 55
}

Granted, this is simply a rewrite of one of Matthias's slides into the @code{let} form with uncurrying, but it really opened up my eyes.

@section{Acknowledgements}

Since our grading policy dictates that we can't look at students' name, I don't know who this student is. Regardless, I would like to thank them for this incredible insight. I also would like to thank Shriram and Matthias for their teaching. Lastly, I thank @link["https://homes.cs.washington.edu/~jrw12/"]{James Wilcox} for his various suggestions for this post.