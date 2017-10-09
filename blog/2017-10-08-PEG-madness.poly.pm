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

Some people really think that this is bad, which could be true, but it's still not too complicated. I can still understand easily why it would not derive some strings.

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

which generates a set of non-empty string of even length with character @code{x}, or, formally @${\set{x^{2n}: n >= 1}}

If we were to write:

@highlight['BNF]|{
  A <- "x" A "x" / "xx"
}|

This generates @${\set{x^{2^n}: n >= 1}}!!!!

This is madness! How could the result be so different!

In particular, the grammar cannot derive @code{xxxxxx} (6 @code{x}). Here, I will show what happens when the parser is trying to parse the string.

@(define step 0)
@(define (visualize-parser . graph-def)
  (set! step (add1 step))
  `(div (div [[id ,(format "parser-viz-~a" step)]])
      (script ,(format "draw(\"~a\", `~a`)" step (apply string-append graph-def)))))

@phantom{
  @script[#:src "https://code.jquery.com/jquery-3.2.1.min.js"]
  @script[#:src "/js/viz.js"]
  @script|{
    function draw(step, graphDef) {
      const element = document.getElementById("parser-viz-" + step);
      const newGraphDef = `
digraph {
  graph [label="Step ${step}", labelloc=t];
  ${graphDef}
}`
      const image = Viz(newGraphDef, {format: "png-image-element"});
      element.appendChild(image);
    }
  }|
}

@(define (rule-1 n)
  (format (string-append "A~a -> x~a_1\n"
                         "A~a -> A~a\n"
                         "A~a -> x~a_2\n") n n n (add1 n) n n))

@(define (declare-rule-1 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a;"
                         "x~a_2[label=\"x\"];") n (add1 n) n))

@(define (declare-rule-1-active n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a[color=blue];"
                         "x~a_2[label=\"x\"];") n (add1 n) n))

@(define (declare-rule-1-defeated-1 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=orange];\n"
                         "A~a;"
                         "x~a_2[label=\"x\"];") n (add1 n) n))

@(define (declare-rule-1-defeated-2 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a[style=\"filled\", fillcolor=orange];"
                         "x~a_2[label=\"x\"];") n (add1 n) n))

@(define (declare-rule-1-defeated-3 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a[style=\"filled\", fillcolor=yellow];"
                         "x~a_2[label=\"x\", style=\"filled\", fillcolor=orange];") n (add1 n) n))

@(define (declare-rule-1-success n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a[style=\"filled\", fillcolor=yellow];"
                         "x~a_2[label=\"x\", style=\"filled\", fillcolor=yellow];") n (add1 n) n))

@(define (declare-rule-1-defeated-2-active n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "A~a[style=\"filled\", fillcolor=orange, color=blue];"
                         "x~a_2[label=\"x\"];") n (add1 n) n))

@(define (rule-2 n)
  (format (string-append "A~a -> x~a_1\n"
                         "A~a -> x~a_2\n") n n n n))

@(define (declare-rule-2-defeated-1 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=orange];\n"
                         "x~a_2[label=\"x\"];") n n))

@(define (declare-rule-2-defeated-2 n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "x~a_2[label=\"x\", style=\"filled\", fillcolor=orange];") n n))

@(define (declare-rule-2-success n)
  (format (string-append "x~a_1[label=\"x\", style=\"filled\", fillcolor=yellow];\n"
                         "x~a_2[label=\"x\", style=\"filled\", fillcolor=yellow];") n n))

@(define (table-series . lst)
  (define col 3)
  (define (get-rows lst)
    (cond
      [(>= (length lst) col) (cons `(tr ,@(map (lambda (x) `(td ,x)) (take lst col)))
                                   (get-rows (drop lst col)))]
      [(cons? lst) (list `(tr ,@(map (lambda (x) `(td ,x)) lst)))]
      [(empty? lst) empty]))
  (define ret `(table ,@(get-rows lst)))
  ret)

@table-series[
  @visualize-parser{
    A1[color=blue];
    A1
  }

  @visualize-parser{
    A1[color=blue];
    @declare-rule-1[1]
    @rule-1[1]
  }

  @visualize-parser{
    A1;
    @declare-rule-1-active[1]
    @rule-1[1]
  }
  @visualize-parser{
    A1;
    @declare-rule-1-active[1]
    @declare-rule-1[2]
    @rule-1[1]
    @rule-1[2]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1-active[2]
    @rule-1[1]
    @rule-1[2]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1-active[2]
    @declare-rule-1[3]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1-active[3]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1-active[3]
    @declare-rule-1[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1-active[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1-active[4]
    @declare-rule-1[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1-active[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1-active[5]
    @declare-rule-1[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1-active[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }
  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1-active[6]
    @declare-rule-1-defeated-1[7]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
    @rule-1[7]
  }
  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1-active[6]
    @declare-rule-2-defeated-1[7]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
    @rule-2[7]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1[5]
    @declare-rule-1-defeated-2-active[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1-active[5]
    @declare-rule-1-defeated-2[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-1[6]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1-active[5]
    @declare-rule-2-defeated-2[6]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
    @rule-2[6]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1[4]
    @declare-rule-1-defeated-2-active[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1-active[4]
    @declare-rule-1-defeated-2[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-1[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1[3]
    @declare-rule-1-active[4]
    @declare-rule-2-success[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-2[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1-active[3]
    @declare-rule-1-defeated-3[4]
    @declare-rule-2-success[5]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-1[4]
    @rule-2[5]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1[2]
    @declare-rule-1-active[3]
    @declare-rule-2-success[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    A1;
    @declare-rule-1[1]
    @declare-rule-1-active[2]
    @declare-rule-1-success[3]
    @declare-rule-2-success[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    A1;
    @declare-rule-1-active[1]
    @declare-rule-1-defeated-3[2]
    @declare-rule-1-success[3]
    @declare-rule-2-success[4]
    @rule-1[1]
    @rule-1[2]
    @rule-1[3]
    @rule-2[4]
  }

  @visualize-parser{
    A1;
    @declare-rule-1-active[1]
    @declare-rule-2-success[2]
    @rule-1[1]
    @rule-2[2]
  }

  @visualize-parser{
    A1[color=blue];
    @declare-rule-1-success[1]
    @declare-rule-2-success[2]
    @rule-1[1]
    @rule-2[2]
  }

  @visualize-parser{
    A1[color=blue, style="filled", fillcolor=yellow];
    @declare-rule-1-success[1]
    @declare-rule-2-success[2]
    @rule-1[1]
    @rule-2[2]
  }
]

However, there are 6 @code{x} in the input string. This only matches 4, so it didn't successfully parse.

The problem, in particular, is that at step 24, @code{A3} should be able to change to rule 2 which would then match the whole string. However, it did not because rule 1 is successful, so we backtrack to @code{A2}, completely ignore the possibility.

Note, however, that it's still possible to generate @${\set{x^{2n}: n >= 1}}.

@highlight['BNF]|{
  A <- "xx" A / "xx"
}|

will do the job.
