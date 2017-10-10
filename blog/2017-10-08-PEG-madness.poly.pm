#lang pollen

@(define-meta title "PEG Madness")
@(define-meta tags (programming-languages parser pyret))

A part of my senior project is to create a framework to allow users to use their own grammar in @link["https://www.pyret.org/"]{Pyret}. A good framework would need to prohibit ambiguous grammar that users might write.

One way to do this would be to require users to write a grammar that is accepted by LALR parser generator, since LALR parser generator will result in a conflict when the grammar is ambiguous. Note that the converse is not true: there could be conflict while the grammar is @emph{not} ambiguous. This is exactly the situation that we are in because Pyret is currently using GLR parser generator which accept all context-free grammars. And Pyret does have some grammar that require unbounded and non-local lookahead, so LALR is not directly applicable.

There are two solutions that I am considering:

@itemlist[
  @item{Rewrite Pyret grammar so that it can be accepted by LALR parser generator. This doesn't seem applicable since Pyret is widely use by a lot of people}
  @item{Use PEG}
]

PEG at first glance seems really nice: linear time parsing, unambiguous by construction, and pretty expressive. However, I found that it could be very deceptive as well.

@see-more

Consider a grammar@numbered-note{Taken from @link["http://www.romanredz.se/papers/FI2008.pdf"]{Some Aspects of Parsing Expression Grammar}, pg. 4}:

@highlight['BNF]|{
  A ::= B "x"

  B ::= "x"
      | "xx"
}|

which generates @${\set{xx, xxx}}

In PEG, there's no choice operator. Instead, we have a @emph{prioritized} choice operator @code{/}, where, if the first branch is successful, the second branch will not be considered.

Because of this, @code{xxx} can't be derived by

@highlight['BNF]|{
  A <- B "x"
  B <- "x" / "xx"
}|

On the other hand, @code{xx} can't be derived by

@highlight['BNF]|{
  A <- B "x"
  B <- "xx" / "x"
}|

Some people really think that this is bad, which could be true. But this is still not too complicated for me. I can still understand easily why it would not derive some strings.

However, this prioritized choice operator has a lot of consequences. It could cause some unexpected parsing error message as follows:

@highlight['BNF]|{
  A <- "foo" / "foobar"
}|

If the input is empty string, the parser will say @code{Line 1, column 1: Expected "foo" or "foobar" but end of input found.}. However, when we enter @code{foobar}, it says @code{Line 1, column 4: Expected end of input but "b" found.}.

But alright, perhaps @code{A <- "foo" / "foobar"} should be considered a bad grammar and I shouldn't write it in the first place.

However, there is a much bigger problem. Consider:

@highlight['BNF]|{
  A ::= "x" A "x"
      | "xx"
}|

which generates a set of non-empty string of even length with character @code{x}, or, formally @${\set{x^{2n}: n \ge 1}}

If we were to write:

@highlight['BNF]|{
  A <- "x" A "x" / "xx"
}|

This generates @${\set{x^{2^n}: n \ge 1}}!!!!

This is madness! How could the result be so different!

In particular, the grammar cannot derive @code{xxxxxx} (6 @code{x}). Here, I will show what happens when the parser is trying to parse the string.

@(define (visualize-parser . graph-def)
  (define unique-id (symbol->string (gensym "parser-viz")))
  `(div (div [[id ,unique-id]])
      (script ,(format "draw(\"~a\", `~a`)" unique-id (apply string-append graph-def)))))

@phantom{
  @script[#:src "/js/viz.js"]
  @script|{
    function draw(uniqueId, graphDef) {
      const newGraphDef = `
digraph {
  ${graphDef}
}`
      const image = Viz(newGraphDef, {format: "png-image-element"});
      document.getElementById(uniqueId).appendChild(image);
    }
  }|
}

@(define (label-x n sub mode)
  (format
    (match mode
      ['normal "x~a_~a[label=\"x\", style=\"bold\"];\n"]
      ['match "x~a_~a[label=\"x\", style=\"filled,bold\", fillcolor=yellow];\n"]
      ['unmatch "x~a_~a[label=\"x\", style=\"filled,bold\", fillcolor=orange];\n"])
    n sub))

@(define (label-A n mode)
  (format
    (match mode
      ['normal "A~a[style=\"bold\"];\n"]
      ['active "A~a[color=blue, style=\"bold\"];\n"]
      ['unmatch "A~a[style=\"filled,bold\", fillcolor=orange];\n"]
      ['unmatch-active "A~a[color=blue, style=\"filled,bold\", fillcolor=orange];\n"]
      ['match "A~a[style=\"filled,bold\", fillcolor=yellow];\n"]
      ['match-active "A~a[color=blue, style=\"filled,bold\", fillcolor=yellow];\n"])
    (add1 n)))

@(define (rule-1 n)
  (format (string-append "A~a -> x~a_1\n"
                         "A~a -> A~a\n"
                         "A~a -> x~a_2\n") n n n (add1 n) n n))

@(define (declare-rule-1 n . args)
  (match args
    [(list) (string-append (label-x n 1 'match) (label-A n 'normal) (label-x n 2 'normal))]
    [(list a b c) (string-append (label-x n 1 a) (label-A n b) (label-x n 2 c))]))


@(define (rule-2 n)
  (format (string-append "A~a -> x~a_1\n"
                         "A~a -> x~a_2\n") n n n n))

@(define (declare-rule-2 n a b)
  (string-append (label-x n 1 a) (label-x n 2 b)))

@(define (table-series . lst)
  (define col 4)
  (define/match (get-col e)
    [((list content i))
      `(td [[valign "top"]]
        (div ,(format "Step ~a" (add1 i))) (div [[align "center"]] ,content))])

  (define (get-rows lst)
    (cond
      [(>= (length lst) col) (cons `(tr ,@(map get-col (take lst col)))
                                   (get-rows (drop lst col)))]
      [(empty? lst) empty]
      [else (list `(tr ,@(map get-col lst)))]))
  `(table ,@(get-rows (for/list ([e lst] [i (length lst)]) (list e i)))))

@table-series[
  @visualize-parser{
    @label-A[0 'active]
    A1
  }

  @visualize-parser{
    @label-A[0 'active]
    @declare-rule-1[1]
    @rule-1[1]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1 'match 'active 'normal]
    @rule-1[1]
  }
  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1 'match 'active 'normal]
    @declare-rule-1[2]
    @rule-1[1]
    @rule-1[2]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2 'match 'active 'normal]
    @rule-1[1]
    @rule-1[2]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2 'match 'active 'normal]
    @declare-rule-1[3]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3 'match 'active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3 'match 'active 'normal]
    @declare-rule-1[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4 'match 'active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4 'match 'active 'normal]
    @declare-rule-1[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5 'match 'active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5 'match 'active 'normal]
    @declare-rule-1[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1[6 'match 'active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }
  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1[6 'match 'active 'normal]
    @declare-rule-1[7 'unmatch 'normal 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
    @rule-1[7]
  }
  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1[6 'match 'active 'normal]
    @declare-rule-2[7 'unmatch 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
    @rule-2[7]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1[6 'match 'unmatch-active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5 'match 'active 'normal]
    @declare-rule-1[6 'match 'unmatch 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5 'match 'active 'normal]
    @declare-rule-2[6 'match 'unmatch]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-2[6]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5 'match 'unmatch-active 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4 'match 'active 'normal]
    @declare-rule-1[5 'match 'unmatch 'normal]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4 'match 'active 'normal]
    @declare-rule-2[5 'match 'match]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-2[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3 'match 'active 'normal]
    @declare-rule-1[4 'match 'match 'unmatch]
    @declare-rule-2[5 'match 'match]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-2[5]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3 'match 'active 'normal]
    @declare-rule-2[4 'match 'match]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1]
    @declare-rule-1[2 'match 'active 'normal]
    @declare-rule-1[3 'match 'match 'match]
    @declare-rule-2[4 'match 'match]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1 'match 'active 'normal]
    @declare-rule-1[2 'match 'match 'unmatch]
    @declare-rule-1[3 'match 'match 'match]
    @declare-rule-2[4 'match 'match]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    @label-A[0 'normal]
    @declare-rule-1[1 'match 'active 'normal]
    @declare-rule-2[2 'match 'match]
    @rule-1[1]
    @rule-2[2]
  }

  @visualize-parser{
    @label-A[0 'active]
    @declare-rule-1[1 'match 'match 'match]
    @declare-rule-2[2 'match 'match]
    @rule-1[1]
    @rule-2[2]
  }

  @visualize-parser{
    @label-A[0 'match-active]
    @declare-rule-1[1 'match 'match 'match]
    @declare-rule-2[2 'match 'match]
    @rule-1[1]
    @rule-2[2]
  }
]

However, there are 6 @code{x} in the input string. This only matches 4, so it didn't successfully parse.

The problem, in particular, is that at step 24, @code{A3} could have changed to rule 2 which would then match the whole string. However, it did not because rule 1 is successful, so we backtrack to @code{A2}, completely ignoring the possibility.

Note, however, that it's still possible to generate @${\set{x^{2n}: n \ge 1}}.

@highlight['BNF]|{
  A <- "xx" A / "xx"
}|

will do the job.
