#lang pollen

@(define-meta title "Undecidability of First-Order Logic")
@(define-meta tags (logic computation))

I have been hearing people saying that first-order logic is undecidable. Last semester, I took both theory of computation and first-order logic classes, and they, too, kept repeating that without proving it. However, now I have basic knowledge of theory of computation and first-order logic. So, fine, I will prove it now!

@see-more

@/h3{Definition}

When we say first-order logic is undecidable, it means a set of all theorems @${\Delta} (formula which are provable) is undecidable. That is, given any formula @${\phi}, there is no Turing machine (abbrev. TM) which will accept if @${\phi} is provable; reject if unprovable. Due to soundness/completeness of first-order logic, this is exactly equivalent to saying that a set of all valid formula (formula which are true under every interpretation) is undecidable.

@/h3{Big Picture}

We will use a standard way to show that first-order logic is undecidable: suppose for the sake of contradiction that first-order logic is decidable, then we will show that some known undecidable problem is decidable, which is absurd. This would then show that first-order logic is in fact undecidable. The known undecidable problem that we will use is going to be the classic @${A_{TM}}: a problem of determining whether or not a TM @${M} accepts string @${w}.

Because we assume that first-order logic is decidable, there is a decider @${D} which can decide whether or not any formula in first-order logic is provable. We then will create a TM @${Z} which will decide whether or not a TM @${M} accepts a string @${w} by using @${D} as a component. The most basic way (which might not always work, but usually work) is let @${Z} be a TM with the following pseudocode:

@pre[#:options '(allowed-math)]{
  @${Z} = on input @${\seq{M, w}}:
    @${f} = ...
    run @${D} on @${f(\seq{M, w})}
    if @${D} accepts, accept
    reject otherwise
}

where @${f} is a computable function transforming an instance of @${A_{TM}} to instance of @${\Delta}. That is, @${f} tranforms @${\seq{M, w}} into some formula @${\phi} where:

@itemlist[
  @item{Whenever @${M} accepts @${w}, @${\phi} is provable. That is, @${D} accepts @${\phi}.}
  @item{Whenever @${M} does not accept @${w}, @${\phi} is unprovable. That is, @${D} rejects @${\phi}.}
]

If we can construct this @${f}, then it means we can create a decider @${Z} to correctly decide an undecide problem, finishing the proof outlined above.

@/h3{Turing Machine}

We first examine the structure of a TM. A TM is defined by @${(Q, \Sigma, \Gamma, \delta, q_0, q_a, q_r)} where:

@itemlist[
  @item{@${Q} is the set of states}
  @item{@${\Sigma} is the set of input alphabets}
  @item{@${\Gamma} is the set of tape alphabets}
  @item{@${\delta} is the transition function}
  @item{@${q_0} is the initial state}
  @item{@${q_a} is the accepting state}
  @item{@${q_r} is the rejecting state}
]

We want @${\phi} to be provable when @${M} accepts @${w} and vice versa. That is when @${M} reaches the state @${q_a} on input @${w}. Reasoning with "@${M} reaches @${q_a} on input @${w}" is quite difficult, because it is an iterative process with many parameters changing in the process. To formalize this and make it easier to reason about, we define a configuration of a TM in a moment to be all parameters that could change: @${(q, T, i)} where:

@itemlist[
  @item{@${q} is a state}
  @item{@${T} is a tape content (which is a string)}
  @item{@${i} is an index on the tape that the header is pointing to}
]

A computation history (abbrev. CH) @${H} is how configurations of a TM change over time. That is, it is a sequence of configurations where:

@numberlist[
  @item{The first configuration @${H_0} is @${(q_0, w, 0)}}
  @item{For any adjacent configurations @${H_j} and @${H_{j+1}}, the configuration @${H_{j + 1}} "follows" from @${H_j}.}
]

Given any @${M} and @${w}, The CH is uniquely determined, as the machine is deterministic. The CH could be either infinite or finite. The former indicates that the TM is stuck in a loop, while the latter indicates that the TM terminates (either accepting or rejecting the input string).

Define an accepting CH to be a CH where the last configuration is at state @${q_a}. To say that @${M} accepts @${w} is nothing more than to say the CH is accepting and vice versa. In a sense, the accepting CH is a @emph{proof} that @${M} accepts @${w}, as it shows how @${M} accepts @${w}.

Therefore, one way to construct @${f} is to transform @${\seq{M, w}} to a formula @${\phi} asserting that the CH on running @${M} on @${w} is accepting. If @${M} accepts @${w}, one can indeed construct the accepting CH, a proof of this formula. If not, then it is impossible to construct the accepting CH.

@/h3{First-Order Logic}

In first-order logic, a formula consists of:

@itemlist[
  @item{
    Logical symbols: have the same semantics universally
    @itemlist[
      @item{Parentheses: @${(, )}}
      @item{Logical connectives: @${\neg, ->, ...}}
      @item{Equality: @${=}. Define @${a \ne b} to be @${\neg (a = b)}}
      @item{Variables: @${v_i} where @${i} is a natural number. For the sake of readability, we allow other characters to be varaibles as well such as @${x} or @${y}}
    ]
  }
  @item{
    Parameters: have different semantics in different interpretations.
    @itemlist[
      @item{Quantifier: @${\forall}. Under interpretation @${I}, it ranges over the non-empty domain @${|I|}. Define @${\exists v. \phi} to be @${\neg \forall v. \neg \phi}}

      @item{
        Functions: such as @${f, g, h}. Each function has a specific arity which is a natural number. Under interpretation @${I}, function @${f} of arity @${k} is assigned @${f^I: |I|^{k} -> |I|} and so on. Notice that when the function @${f} has arity @${0}, @${f} degenerates to a constant.

        For example, consider functions @${\hat{+}}, @${\hat{0}}, and @${\hat{1}} of arity 2, 0, and 0 respectively. Under some interpretations where:

        @itemlist[
          @item{@${|I| = \R}}
          @item{@${\hat{+}^I: a, b |-> a + b}}
          @item{@${\hat{0}^I = 1}}
          @item{@${\hat{1}^I = 2}}
        ]

        Both formula @${\hat{+}(\hat{0}, \hat{1}) = \hat{+}(\hat{1}, \hat{0})} and @${\hat{+}(\hat{0}, \hat{0}) = \hat{1}} are true.

        @item{Predicates: such as @${P, Q, R}. Each predicate has a specific arity which is a natural numbers. Under interpretation @${I}, predicate @${P} of arity @${k} is assigned @${P^{I} \subseteq |I|^{k}} and so on.

        For example, consider predicate @${P} of arity 3 and functions @${\hat{0}}, @${\hat{1}}, and @${\hat{2}} of arity 0. Under some interpretations where:

        @itemlist[
          @item{@${|I| = \Q}}
          @item{@${P^I = \setof{(a, b, c)}{a < b < c}}}
          @item{@${\hat{0}^I = 0}}
          @item{@${\hat{1}^I = 1}}
          @item{@${\hat{2}^I = 2}}
        ]

        The formula @${P(\hat{0}, \hat{1}, \hat{2})} is true, while @${P(\hat{2}, \hat{1}, \hat{0})} is not.

        Under another interpretations @${I'} which is just like @${I} except that @${P^{I'} = \setof{(a, b, c)}{a > b > c}}, the formula @${P(\hat{2}, \hat{1}, \hat{0})} is true, while @${P(\hat{0}, \hat{1}, \hat{2})} is not.}
      }
    ]
    Parameters are need to be specified when defining a language. For example, a language of set theory has parameter @${\forall} and @${\in} with arity 2.
  }
]

From now on, we will use the infix notation when applicable for the sake of readability. This applies to both functions and predicates. For example, we will write @${a + b} and @${a \in b} instead of @${+(a, b)} and @${\in(a, b)}

@/h3{Establishing a Correspondence}

As mentioned above, the function @${f} that we will create will produce a formula @${\phi} which asserts that the CH is accepting when running @${M} on @${w}. Therefore, consider an interpretation where the domain is the set of configurations. The formula will establish rules: how the initial configuration should be, how configurations will transition, and force that the last configuration must be in the accepting state.

The rules is all about configurations in the CH when running @${M} on @${w}, so we want to be able to precisely state this in the formula. That is, we need a predicate stating that an object is a configuration in the CH. But first of all, we need to decide first what should be the domain of the objects.

Because a configuration consists of @${(q, T, i)}, one possible way is to have states, strings, and integers in the domain. However, this is going to be very complex. Notice that any TM has finite states, so we in fact can encode the facts about states using the formula instead. Now we only have strings and integers left to be in the domain. However, note that another way to specify the current cell in the tape is to say that the tape consists of two parts: left part and right part, where the current cell is exactly the first cell of the right part. The whole tape then is just the concatenation of two parts. With this way, there is no need to keep integers around; we only need strings to be in the domain.

@/h4{Strings in First-Order Logic}

Let's say that we want to encode string over the set of alphabets @${\Sigma} in first-order logic. For instance, let @${\Sigma} be the set of 26 English alphabets. We at least want to be able to construct the object @code{hello}. How can we do it?

We notice that a string is just a list of characters which can be defined recursively: a list of length @${n} is either an empty list or a list of length @${n - 1} appended by one more element. Similarly, a string of length @${n} is either an empty string or a string of length @${n - 1} together with one more character at the back. Therefore, define a constant (a function with arity 0) @${\epsilon} to represent empty string. Define functions @${a_a, a_b, ..., a_z} with arity 1, where @${a_\alpha(x)} is to put a character @${\alpha} at the back of @${x}. Then @code{hello} is just @${a_o(a_l(a_l(a_e(a_h(\epsilon)))))}.

Note that in other interpretations, it could be the case that all @${a_\alpha} are just identity functions, and @${\epsilon} is 3. @${\epsilon} being 3 is not that much of a problem, cause 3 might mean empty string. The problem is that in these interpretations all strings are the same. To encode strings properly, we need to have more conditions: @${\forall x. a_\alpha(x) \ne a_\beta(x)} for all @${\alpha, \beta \in \Sigma, \alpha \ne \beta}

@/h4{Define Predicates}

Construct @${|Q|} predicates where @${R_q} is a predicate with arity 2 for each @${q \in Q}. The interpretation of @${R_q(l, r)} is, @${(q, l \circ r, |l|)} is a configuration in the CH.

The rest becomes obvious: to say that the last configuration is in the accepting state is: @${\exists l. \exists r. R_{q_a}(l, r)}. To set up the first configuration is: @${R_{q_0}(\epsilon, w)}.

Wait, what is @${\epsilon} and @${w}? @${\epsilon} would be just a constant (function with arity 0) in this language, intended to mean an empty string. @${w} is the input. We could make it a constant, but under some interpretations, it's unlikely that this constant to correspond to the actual @${w} that we have. Instead, we will build
