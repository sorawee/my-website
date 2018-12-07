#lang pollen

@(define-meta title "Deriving the Y Combinator (Part 1)")
@(define-meta tags (programming-languages y-combinator DRAFT))
@(define-meta updated (2018 12 7))

The Y combinator is a somewhat magical aspect of the untyped lambda calculus. Many people presented explanations of this magic, but I found them somewhat unsatisfactory. Some explanations start by showing the combinator in the beginning and then simply demonstrate that the given combinator is correct. Some other explanations contain lines like "Why don't we try passing it to itself? And look, this turns out to be exactly what we want!" These explanations don't give an intuition for how one could @emph{constructively} obtain the combinator. @emph{What on earth makes you think that passing it to itself is a good idea?} As such, this post, intended for those who know the Y combinator already but do not satisfy with the existing explanations, attempts to explain how one could constructively derive the Y combinator.

@see-more

@section{Introduction}

In @link["https://en.wikipedia.org/wiki/Lambda_calculus"]{lambda calculus}, there's no primitive support to define recursive functions. @emph{Can we define recursive functions in lambda calculus regardless? And how?}

While our question focuses on lambda calculus, to make it easier to follow, we will use an actual programming language. Here, I choose to use @link["http://racket-lang.org"]{Racket}@margin-note{The choice of Racket is that:
@inline-itemlist[
  @item{It is not statically typed, so we can write the combinator which would have been prohibited had we use a language with a standard type system like @link["https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system"]{Hindley–Milner}.}
  @item{Its scoping rule is simple and close to lambda calculus. Unlike, say, JavaScript and Python, there's no variable hoisting, so we won't encounter a problem where a simple, innocent code @code{const f = () => f();} unintentionally allows recursion.}
]}, but other programming languages might work as well.

As mentioned above, this post is intended for those who know basic programming language theory and the Y combinator already. However, I will try to provide links as many as possible to accommodate non-target audience.

@section{Enabling recursion}

In Racket, we can write code to compute @${3! + 4!} easily.

@highlight['racket]{
(letrec ([fact (λ (n)
                 (match n
                   [0 1]
                   [_ (* n (fact (- n 1)))]))])
  (+ (fact 3) (fact 4)))
; => 30
}

Defining this recursive @code{fact} function is possible because @link["https://docs.racket-lang.org/guide/let.html#%28part._.Recursive_.Binding__letrec%29"]{@code{letrec}}@margin-note{Read @code{(letrec ([@mvar{x} @mvar{v}]) @mvar{e})} as "let @code{@mvar{x}} be a recursive function @code{@mvar{v}} and then return @code{@mvar{e}}"} allows us to refer to @code{fact} inside the lambda function that @emph{will} be @code{fact} itself.

Of course, lambda calculus has no @code{letrec}. We might ask how Racket implements @code{letrec}, and the short answer is that it does so by using mutation, which lambda calculus doesn't have either. To simulate lambda calculus, we will write the factorial function in Racket without using @code{letrec} and mutation. Here's our first straightforward attempt:

@highlight['racket]|{
(let ([fact (λ (n)
              (match n
                [0 1]
                [_ (* n (⟦focus fact ⟧ (- n 1)))]))])
  (+ (fact 3) (fact 4)))
;; => fact: unbound identifier in: fact
}|

We simply change @code{letrec} to @link["https://docs.racket-lang.org/guide/let.html#%28part._.Parallel_.Binding__let%29"]{@code{let}}@margin-note{Read @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} as "let @code{@mvar{x}} be @code{@mvar{v}} and then return @code{@mvar{e}}"}. Although @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} and @code{(letrec ([@mvar{x} @mvar{v}]) @mvar{e})} are very similar, @code{let} doesn't have the ability that allows us to refer to @code{@mvar{x}} in @code{@mvar{v}}. In fact, @code{let} doesn't add any expressive power to lambda calculus at all because its semantics is completely equivalent to @code{((λ (@mvar{x}) @mvar{e}) @mvar{v})}, a lambda term. Some programming languages simply treats @code{let} as a @link["https://en.wikipedia.org/wiki/Syntactic_sugar"]{syntactic sugar}, and desugar @code{(let ([@mvar{x} @mvar{v}]) @mvar{e})} to @code{((λ (@mvar{x}) @mvar{e}) @mvar{v})}.

Our first straightforward attempt doesn't work because the highlighted @code{fact} is unbound inside the lambda function. How can we fix it? Let's take a look at another example:

@highlight['racket]|{
(let ([f (λ (n) (+ n ⟦focus c ⟧))])
  (let ([c 1])
    (f 99)))
;; => c: unbound identifier in: c
}|

The above code has a similar problem. @code{c} is unbound inside the body of the lambda. One way to fix the problem is to explicitly pass @code{c} to the lambda function, so that @code{c} is bound:

@highlight['racket]|{
(let [[f (λ (⟦add c ⟧ n) (+ n c))]]
  (let [[c 1]]
    (f ⟦add c ⟧ 99)))
;; => 100
}|

Our solution to the unbound @code{fact} problem is going to be the same. While @code{fact} is unbound inside the lambda function, it is bound in the body of @code{let}, so we can pass @code{fact} explicitly to the lambda function!

@highlight['racket]|{
(let ([fact (λ (⟦add fact ⟧ n)
              (match n
                [0 1]
                [_ (* n ⟦focus (fact (- n 1)) ⟧)]))])
  (+ (fact ⟦add fact ⟧ 3) (fact ⟦add fact ⟧ 4)))
;; => fact: arity mismatch;
;; the expected number of arguments does not match the given number
;;  expected: 2
;;  given: 1
;;  arguments...:
}|

There's still a problem: @code{fact} is now a function of arity 2, so the highlighted function application would result in an arity mismatch error. What does @code{fact} need as the first argument? The answer is @code{fact}! So we can fix the code accordingly:

@highlight['racket]|{
(let ([fact (λ (fact n)
              (match n
                [0 1]
                [_ (* n (fact ⟦add fact ⟧ (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
;; => 30
}|

With this trick, we can write any recursive function in a language without @code{letrec} by the following method:

@numberlist[
  @item{Initially write the recursive function @${f} with @code{letrec}.}
  @item{Change @code{letrec} to the @code{let}.}
  @item{At every callsite of @${f}, prepend the arguments with @${f}@|apos|s name}
  @item{Prepend the parameters of @${f} with @${f}@|apos|s name}
]

This is simply a syntax transformation which could be done programmatically.

@section{Omega}

Let's digress a little bit here. What will happen if we perform the syntax transformation to the stupid infinite loop function?

@highlight['racket]|{
(letrec ([diverge (λ () (diverge))])
  (diverge))
}|

is transformed to:

@highlight['racket]|{
(let ([diverge (λ (diverge) (diverge diverge))])
  (diverge diverge))
}|

To obtain a lambda term, we simply desugar @code{let} as described above:

@highlight['racket]|{
  ((λ (diverge) (diverge diverge)) (λ (diverge) (diverge diverge)))
}|

Recall that this is known as the @${\Omega}-combinator, a classic example of a lambda term whose evaluation doesn't terminate.

@section{Deriving a fake Y combinator}

While we now know how to write recursive functions in lambda calculus, the transformed function doesn't look like the original function that we would write in the @code{letrec} version. It would be nice if there is a term @code{@mvar{make-recursion-possible}} such that:

@highlight['racket]{
(@mvar{make-recursion-possible}
  (match n
    [0 1]
    [_ (* n (⟦focus fact ⟧ (- n 1)))]))
}

produces the actual factorial function. However, this clearly won't work, because the highlighted identifiers are going to be unbound. To fix this, we simply make it bound by wrapping a lambda function around the body.

@highlight['racket]{
(@mvar{make-recursion-possible}
  (⟦add λ (fact) ⟧
    (match n
      [0 1]
      [_ (* n (fact (- n 1)))])))
}

We will call the lambda function that is the argument of @code{@mvar{make-recursion-possible}} a @emph{recursion maker}. And what we want is to find @code{@mvar{make-recursion-possible}} so that for any @code{@mvar{recursion-maker}}, @code{(@mvar{make-recursion-possible} @mvar{recursion-maker})} produces the actual recursive function.

How should we approach this? We don't know what @code{@mvar{make-recursion-possible}} looks like. But for the factorial function, we do know what @code{@mvar{fact-maker}} looks like. The strategy, then, is to extract the @code{@mvar{fact-maker}}:

@highlight['racket]|{
(λ (fact)
  (match n
    [0 1]
    [_ (* n (fact (- n 1)))]))
}|

from our current code:

@highlight['racket]|{
(let ([fact (λ (fact n)
              (match n
                [0 1]
                [_ (* n (fact fact (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
}|

Hopefully, what's left is going to be @code{@mvar{make-recursion-possible}}

As a first step, we can avoid prepending @code{fact} at the callsites of @code{fact} inside the lambda function. Simply transform:

@highlight['racket]|{
(let ([fact (λ (fact n)
              (match n
                [0 1]
                [_ (* n (fact fact (- n 1)))]))])
  (+ (fact fact 3) (fact fact 4)))
}|

to

@highlight['racket]|{
(let ([fact (λ (fact n)
              (let ([fact (λ (n) (fact fact n))])
                ⟦focus (match n
                  [0 1]
                  [_ (* n (fact (- n 1)))]) ⟧))])
  (+ (fact fact 3) (fact fact 4)))
}|

which shadows @code{fact} that consumes two arguments with @code{fact} that consumes one argument while maintaining the semantics. The body of the function is now very similar to @code{@mvar{fact-maker}}, especially for the highlighted part. What can we do next? We see that @code{fact} and @code{n} are both the formal parameters in the transformed code, whereas in @code{@mvar{fact-maker}} they are separated. Straightforwardly, we can use @link["https://en.wikipedia.org/wiki/Currying"]{currying} to separate the two apart. We of course need to change how we call the function as well:

@highlight['racket]|{
(let ([fact (λ (fact)
              (λ (n)
                (let ([fact (λ (n) ((fact fact) n))])
                  (match n
                    [0 1]
                    [_ (* n (fact (- n 1)))]))))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}|

The @code{(let ([fact (λ (n) ((fact fact) n))]) ...)} doesn't depend on the parameter @code{n}, so we can lift it up without changing the semantics:

@highlight['racket]|{
(let ([fact (λ (fact)
              (let ([fact (λ (n) ((fact fact) n))])
                ⟦focus (λ (n)
                  (match n
                    [0 1]
                    [_ (* n (fact (- n 1)))])) ⟧))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}|

Now our highlighted code is very similar to the @code{@mvar{fact-maker}}! What we want to do next is to somehow convert @code{(let ([fact ...]) body...)} into @code{(λ (fact) body...)} and move @code{(λ (n) ((fact fact) n))} somewhere else. That's simply desugaring @code{let}!

@highlight['racket]|{
(let ([fact (λ (fact)
              (⟦focus (λ (fact)
                 (λ (n)
                   (match n
                     [0 1]
                     [_ (* n (fact (- n 1)))]))) ⟧
               (λ (n) ((fact fact) n))))])
  (+ ((fact fact) 3) ((fact fact) 4)))
}|

and we finally have the @code{@mvar{fact-maker}}!

Our next mission is to obtain the @code{@mvar{make-recursion-possible}}. To make things tidy, let's first abstract @code{@mvar{fact-maker}} out as a variable named @code{fact-maker}:

@highlight['racket]|{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (match n
                        [0 1]
                        [_ (* n (fact (- n 1)))])))])
  (let ([fact (λ (fact) (fact-maker (λ (n) ((fact fact) n))))])
    (+ ((fact fact) 3) ((fact fact) 4))))
}|

Recall that @code{(@mvar{make-recursion-possible} fact-maker)} should produce the actual factorial function which consumes only one number as an argument. Right now we still have @code{((fact fact) 3)} and @code{((fact fact) 4)} in the innermost body as a result of argument list prepending. We can fix this using the same transformation that we did earlier:

@highlight['racket]|{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (match n
                        [0 1]
                        [_ (* n (fact (- n 1)))])))])
  (let ([fact (λ (fact) (fact-maker (λ (n) ((fact fact) n))))])
    (let ([fact (fact fact)])
      (+ (fact 3) (fact 4)))))
}|

As we want a single term for @code{@mvar{make-recursion-possible}}, we probably don't want nested @code{let}, so we will fuse them together in a straightforward way:

@highlight['racket]|{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (match n
                        [0 1]
                        [_ (* n (fact (- n 1)))])))])
  (let ([fact ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
               (λ (fact) (fact-maker (λ (n) ((fact fact) n)))))])
    (+ (fact 3) (fact 4))))
}|

As a final step, @code{@mvar{make-possible-recursion}} should consume @code{fact-maker} as an argument, so we can abstract @code{fact-maker} out:

@highlight['racket]|{
(let ([fact-maker (λ (fact)
                    (λ (n)
                      (match n
                        [0 1]
                        [_ (* n (fact (- n 1)))])))])
  (let ([fact ((λ (fact-maker)
                 ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
                  (λ (fact) (fact-maker (λ (n) ((fact fact) n))))))
               fact-maker)])
    (+ (fact 3) (fact 4))))
}|

And we are done! We have derived the @code{@mvar{make-recursion-possible}} as follows:

@highlight['racket]|{
(λ (fact-maker)
  ((λ (fact) (fact-maker (λ (n) ((fact fact) n))))
   (λ (fact) (fact-maker (λ (n) ((fact fact) n))))))
}|

Notice that there's nothing inherent about the factorial function here. If we use the Fibonacci function as a starting point, we would end up with a term that is @link["https://en.wikipedia.org/wiki/Lambda_calculus#Alpha_equivalence"]{@${\alpha}-equivalent} to this @code{@mvar{make-recursion-possible}} as well. We will highlight this fact by @${\alpha}-convert variable names to remove all references to the factorial function:

@highlight['racket]|{
(λ (f)
  ((λ (x) (f (λ (v) ((x x) v))))
   (λ (x) (f (λ (v) ((x x) v))))))
}|

This @code{@mvar{make-recursion-possible}} is formally called the @emph{Z combinator}, and we will subsequently call it @${Z}. It is one of the well-known @emph{fixed-point combinator}s. The word @emph{combinator} means that there's no free variable in @${Z}. The word @emph{fixed-point} means that @${Z(f)} is a solution to the equation @${k = f(k)} for any function @${f}. That is, @${Z(f) = f(Z(f))}. Note that we can keep continue going, and we will obtain @${Z(f) = f(Z(f)) = f(f(Z(f))) = f(f(f(Z(f)))) = \dots}. In our example @code{Z(fact-maker)} is equivalent to @code{fact-maker(fact-maker(fact-maker(...)))}. We will investigate what this means in the next post.

Recall that the title of this section is @emph{Deriving a fake Y combinator}. It's fake because we actually derived the Z combinator, not the Y combinator. However, they are essentially the same. The difference of the two combinators will also be covered in the next post.

Here's our final code:

@highlight['racket]|{
(let ([Z (λ (f)
           ((λ (x) (f (λ (v) ((x x) v))))
            (λ (x) (f (λ (v) ((x x) v))))))])
  (let ([fact-maker (λ (fact)
                      (λ (n)
                        (match n
                          [0 1]
                          [_ (* n (fact (- n 1)))])))])
    (let ([fact (Z fact-maker)])
      (+ (fact 3) (fact 4)))))
;; => 30
}|

@section{Y and Me}

I learned the Y combinator two years ago from the @link["https://cs.brown.edu/courses/cs173/"]{PL class} taught by @link["https://cs.brown.edu/~sk/"]{Shriram Krishnamurthi}. One great explanation (which Shriram pointed us to) is @link["http://felleisen.org/matthias/"]{Matthias Felleisen}'s @link["https://xivilization.net/~marek/binaries/Y.pdf"]{@italic{A Lecture of the Why of Y}}, which similarly attempts to derive the Y combinator. However, the explanation that corresponds to the "Enabling recursion" section goes in a much slower pace: it started by defining a series of functions @${\set{f_i}} where @${f_i} is the factorial function that works on input less than or equal to @${i}, then abstracted out common patterns, and then performed the "self-application" trick. A strength of this approach is that it crucially uses the fixed-point identity @${Y(f) = f(Y(f))} to justify the self-application trick in the derivation, so it motivates really well why @${Y(f) = f(Y(f))} is important. The approach that I use, on the other hand, simply focuses on where things are bound and how do we direct values to appropriate places, which results in a simpler explanation in my opinion. I also make use of @code{let} a lot, which I think helps a lot with readability, with the weakness being that I also need to talk about desugaring.

I didn't discover this approach myself. The core insight of this approach is from a student's homework that I graded in the @link["https://cs.brown.edu/courses/cs173/2017/"]{current iteration} of the PL class. A part of the homework asks students to write random programs in a language that is very similar to Racket without @code{letrec} and mutation. I didn't expect that students will be able to write a really meaningful program unless they know a fixed-point combinator already, ... or so I thought. It turns out that one student wrote a program to calculate 1 + 2 + ... + n for arbitrary n without knowing a fixed-point combinator! Here's the program in Racket:

@highlight['racket]|{
(let ([triangle (λ (triangle n)
                  (match n
                    [0 0]
                    [_ (+ n (triangle triangle (- n 1)))]))])
  (triangle triangle 10))
;; => 55
}|

Granted, this is simply a rewrite of one of Matthias's slides into the @code{let} form with uncurrying, but it really opened up my eyes.

@section{Acknowledgements}

Since our grading policy dictates that we can't look at students' name, I don't know who this student is. Regardless, I would like to thank them for this incredible insight. I also would like to thank Shriram and Matthias for their teaching. Lastly, I thank @link["https://homes.cs.washington.edu/~jrw12/"]{James Wilcox} for his various suggestions of this post.