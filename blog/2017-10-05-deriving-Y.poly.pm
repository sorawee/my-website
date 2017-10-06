#lang pollen

@(define-meta title "Deriving Y combinator")
@(define-meta tags (programming-languages math))

Y combinator seems to be something mysterious. @link["https://cs.stackexchange.com/questions/9604/clear-intuitive-derivation-of-the-fixed-point-combinator-y-combinator"]{Several} people have been trying to understand the intuition why it works. I have seen "@link["https://xivilization.net/~marek/binaries/Y.pdf"]{A Lecture on the Why of Y}" by Matthias Felleisen before, and while it does give me some insight, I still feel confused in some degree. That doesn't prevent me from recommending this lecture to other people I talked to because it's the best one I have seen so far.

Recently I thought about the fixed point combinator again and finally gained some good insight. I think it's worth sharing here.

@see-more[]

In lambda calculus, also known as the language of functions, an expression @code{e} is either:

@itemlist[
  @item{@code{x}; identifier}
  @item{@code{(lambda (x) e)}; abstraction}
  @item{@code{(e e)}; application}
]

Church and Turing proved that lambda calculus is Turing complete. That means we should at least be able to write a factorial function in this language. Consider the factorial function in Racket:

@highlight['racket]|{
  (define fact
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (sub1 n))))))
  (fact 10)
}|

We seem to need numbers, basic arithmetics, booleans, number comparison, conditionals, and @emph{a way to perform recursion}@margin-note{@code{define} does not simply define a function. It also makes recursion possible} in order to write the factorial function.

@link["https://en.wikipedia.org/wiki/Church_encoding"]{Church encoding} provides a way to write numbers, booleans, and all that, in the language, @emph{except} a way to perform recursion. For the sake of readability, we will use primitive numbers, booleans, etc. as we know a way to convert them to lambda terms if we want to. Similarly, @code{(let [[x v]] e)} is simply a syntactic sugar for @code{((lambda (x) e) v)} which is a lambda term, so if using @code{let} will make the code clearer, we will.@margin-note{I use double bracket on @code{let} just to let us distinguish it with other parenthesis easier -- actually the preferable style is to write it as @code{(let ([x v]) e)}.}

Now, the goal is to write the factorial function without using @code{define} which gives a power to define a recursive function. Here's the first straightforward attempt:

@highlight['racket]|{
  (let [[fact
         (lambda (n) (if (zero? n)
                         1
                         (* n (fact (sub1 n)))))]]
       (fact 10))
}|

But this doesn't work because with static scope, @code{fact} is unbound inside the lambda function that is going to be @code{fact} itself.

The key idea is that, while @code{fact} is unbound inside the lambda function, it is bound in the body of @code{let}, so why don't we give @code{fact} explicitly to the function?@margin-note{A code from a student in a class that I am TA'ing gave me this idea. I'm kinda embarassed that I did not consider this before}

@highlight['racket]|{
  (let [[fact
         (lambda (fact n) (if (zero? n)
                          1
                          (* n (fact (sub1 n)))))]]
    (fact fact 10))
}|

@margin-note{Lambda calculus actually doesn't support multi arity lambda functions, but we can use @link["https://en.wikipedia.org/wiki/Currying"]{currying} to circumvent the problem, so again, we will continue using multi arity functions for now, for the sake of readability.}

Oh, we forgot to pass @code{fact} in the recursive call to match the new formal parameters! Let's fix that:

@highlight['racket]|{
  (let [[fact
         (lambda (fact n) (if (zero? n)
                          1
                          (* n (fact fact (sub1 n)))))]]
    (fact fact 10))
}|

And, ta-dah! It works!

With this trick, we can write any recursive function in lambda calculus by the following method:

@numberlist[
  @item{Initially write the recursive function with @code{define}.}
  @item{Change it to the @code{let} form.}
  @item{Prepend both formal and actual parameters of the function with the a new parameter which uses the name of the function.}
]

This transformation is simply a syntax transformation which could be done programmatically,

Let's digress a little bit here. What will happen if I do this to the stupid infinite loop function?

@highlight['racket]|{
  (define (diverge) (diverge))
  (diverge)
}|

is transformed to:

@highlight['racket]|{
  (let [[diverge (lambda (diverge) (diverge diverge))]] (diverge diverge))
}|

Now we desugar @code{let} back to @code{lambda} and application to obtain:

@highlight['racket]|{
  ((lambda (diverge) (diverge diverge)) (lambda (diverge) (diverge diverge)))
}|

which is the other mysterious, well-known @${\Omega} combinator!

Now, back to the main topic, the question that we should ask next is, are we satisfied with this way to write an arbitrary recursive function in lambda calculus? As it turned out, some people are dissatisfied because the transformed function doesn't look like the original function that we would write in the @code{define} version. In fact, their goal is to try to make the transformed function look close enough to the original function, and @emph{abstract} the part that looks like the original factorial function out. The result is a function that enables other functions to be recursive while meintaining their original form.

As a first step, we can transform:

@highlight['racket]|{
  (let [[fact
         (lambda (fact n) (if (zero? n)
                          1
                          (* n (fact fact (sub1 n)))))]]
    (fact fact 10))
}|

to

@highlight['racket]|{
  (let [[fact
         (lambda (fact n)
           (let [[fact (lambda (n) (fact fact n))]]
             (if (zero? n)
                 1
                 (* n (fact (sub1 n))))))]]
    (fact fact 10))
}|

which shadows @code{fact} that consumes two arguments with @code{fact} that consumes one argument while maintaining the semantics.

The body of the function is in fact now very similar to the original function that uses @code{define}. What can we do next? We see that @code{fact} and @code{n} are in the formal parameters in the transformed code, while only @code{n} is in the original one. Currying which is mentioned in the margin note above could separate two of them. Of course, we need to change how we call the function too.

@highlight['racket]|{
  (let [[fact
         (lambda (fact)
           (lambda (n)
             (let [[fact (lambda (n) ((fact fact) n))]]
               (if (zero? n)
                   1
                   (* n (fact (sub1 n)))))))]]
    ((fact fact) 10))
}|

The @code{(let [[fact ...]])} inside the lambda does not depend on the parameter @code{n} of the outter lambda function, so we can lift it up without changing the semantics:

@highlight['racket]|{
  (let [[fact
         (lambda (fact)
           (let [[fact (lambda (n) ((fact fact) n))]]
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n)))))))]]
    ((fact fact) 10))
}|

And we see the original code for the @code{fact} function in the above code now!

Are we done? Well, not quite, because even though this resembles the original factorial function, we can't abstract:

@highlight['racket]|{
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n)))))
}|

out as @code{fact} would then be unbound. But if we were to abstract the whole:

@highlight['racket]|{
  (lambda (fact)
    (let [[fact (lambda (n) ((fact fact) n))]]
      (lambda (n)
        (if (zero? n)
            1
            (* n (fact (sub1 n)))))))
}|

That would be too ugly.

What can we do? We see a @code{let}, so let's desugar it again:

@highlight['racket]|{
  (let [[fact
         (lambda (fact)
           ((lambda (fact)
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n))))))
            (lambda (n) ((fact fact) n))))]]
    ((fact fact) 10))
}|

!!!!! Now we obtain:

@highlight['racket]|{
  (lambda (fact)
    (lambda (n)
      (if (zero? n)
          1
          (* n (fact (sub1 n))))))
}|

which is the smallest part that can be abstracted out! Let's call it @code{F}. Next, we have:


@highlight['racket]|{
  (let [[F (lambda (fact)
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n))))))]]
    (let [[fact
           (lambda (fact)
             (F (lambda (n) ((fact fact) n))))]]
      ((fact fact) 10)))
}|

Now, substitute @code{fact} into the body of @code{let}:

@highlight['racket]|{
  (let [[F (lambda (fact)
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n))))))]]
    (((lambda (fact) (F (lambda (n) ((fact fact) n))))
      (lambda (fact) (F (lambda (n) ((fact fact) n))))) 10))
}|

If we want to truly abstract @code{F} out, we will not want it to be free in the body of @code{let}, so:

@highlight['racket]|{
  (let [[F (lambda (fact)
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n))))))]]
    (((lambda (F)
        ((lambda (fact) (F (lambda (n) ((fact fact) n))))
         (lambda (fact) (F (lambda (n) ((fact fact) n)))))) F) 10))
}|

The fragment:

@highlight['racket]|{
  (lambda (F)
    ((lambda (fact) (F (lambda (n) ((fact fact) n))))
      (lambda (fact) (F (lambda (n) ((fact fact) n))))))
}|

or, after renaming while maintaining alpha equivalence:

@highlight['racket]|{
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v))))))
}|

is known as Y combinator. Let's now use the name @code{Y} instead:

@highlight['racket]|{
  (let [[F (lambda (fact)
             (lambda (n)
               (if (zero? n)
                   1
                   (* n (fact (sub1 n))))))]
        [Y (lambda (f)
             ((lambda (x) (f (lambda (v) ((x x) v))))
              (lambda (x) (f (lambda (v) ((x x) v))))))]]
    ((Y F) 10))
}|

And just to clean things up to match Matthias's lecture:

@highlight['racket]|{
  (let [[fact-maker (lambda (fact)
                      (lambda (n)
                        (if (zero? n)
                            1
                            (* n (fact (sub1 n))))))]
        [Y (lambda (f)
             ((lambda (x) (f (lambda (v) ((x x) v))))
              (lambda (x) (f (lambda (v) ((x x) v))))))]]
    (let [[fact (Y fact-maker)]]
      (fact 10)))
}|

The process, if you notice, does not really depend on the fact that we use the factorial function, so in fact, @code{Y} enables recursive functions on every maker.

My presentation is similar to Matthias's lecture a lot, but Matthias's seems to insist on using @code{lambda} everywhere which might be a little bit confusing. I find that using @code{let} make things clearer a lot. Matthias's also seems to have weird tower of makers and @emph{hukairs} which I don't really understand.
