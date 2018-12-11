#lang pollen

@(define-meta title "Yet Another Coq Tactic Index")
@(define-meta type page)

@title[]

@section{Inverted Tactic Index}

This section is organized according to a combination of proof state and intention rather than tactic names.

@subsection{Solving based on the shape of the goal}
@lookup-tac["reflexivity"]
@lookup-tac["constructor"]

@subsection{Solving by absurdity}
@lookup-tac["discriminate"]

@subsection{Uncategorized}
@lookup-uncat[]

@section{Tactic Index}

This section is organized according to tactic names.

@coq-tactic{reflexivity}

@usage{@tac{}}

@use-when{you want to @emph{solve} the goal that is a @emph{true equality}. I.e., the goal has the form @code{@mvar{t} = @mvar{u}} and both @code{@mvar{t}} and @code{@mvar{u}} are equal syntactically after some computation.}

@additional-desc{The tactic calls @tactic{intros} automatically before the actual solve.}

In this example we will apply @tactic{reflexivity} to prove that for every natural number @${x}, if @${1 = 1} then @${(3 + 2) + x = (2 + 3) + x}.

@coq-interactive{
Lemma complex_math :
  forall (x : nat),
    1 = 1 ->
    (3 + 2) + x = (2 + 3) + x.
Proof.
  intros.
  @mark["focus"]{reflexivity.}
  (* reflexivity alone
     (without intros) also works *)
Qed.
}

Note that while Coq can compute concrete values, making it possible to reason that both @code{(3 + 2) + x} and @code{(2 + 3) + x} reduce to @code{5 + x} and therefore are equal, it cannot compute non-concrete values. Hence, @tac{} can't prove the goal that needs non-trivial reasoning, like commutativity of natural numbers that involves non-concrete values.

@coq-interactive{
Lemma commutativity_is_not_trivial :
  forall (x : nat), 1 + x = x + 1.
Proof.
  intros.
  @mark["focus"]{reflexivity.}
}

The above lemma can be proven by performing an @tactic{induction} on @code{x}, or by applying lemmas like @code{Nat.add_comm}.

@relevant-tactics[
  @item{Equivalent to @tactic{exact eq_refl.}}
  @item{Equivalent to @tactic{apply eq_refl.}}
]

@coq-tactic{assumption}

@usage{@tac{}}

@use-when{you want to @emph{solve} the goal that existed already in the context after some computation.}

In this example we will use @tac{} to prove that if @${x = (1 + 2) + y}, then @${x = 3 + y}.

@coq-interactive{
Lemma another_complex_math :
  forall (x y : nat),
    x = (1 + 2) + y ->
    x = 3 + y.
Proof.
  intros.
  @mark["focus"]{assumption.}
  (* Use "H: x = 1 + 2 + y" *)
Qed.
}

@relevant-tactics[
  @item{Equivalent to @tactic{exact @mvar{H}.} where @code{@mvar{H}} is a hypothesis in the context that matches the goal.}
  @item{Equivalent to @tactic{apply @mvar{H}.}}
]

@coq-tactic{discriminate}

@usage{@tac{}}

@use-when{you want to @emph{solve} any goal because the context contains a @emph{false equality} due to @emph{mismatched constructors} after some computation.}

@additional-desc{The tactic calls @tactic{intros} automatically before the actual solve.}

In this example we will use @tac{} to prove that for any natural @${y}, if @${1 = 2 + y}, then any proposition is true. Of course, the premise can't be true because the lowest possible value of @${y} is @${0}, and even then @${1 < 2 + 0}.

@coq-interactive{
Lemma mismatch_proves_anything :
  forall (y : nat) (P : Prop),
    1 = 2 + y -> P.
Proof.
  intros.
  @mark["focus"]{discriminate.}
  (* Use "H: 1 = 2 + y" *)
  (* discriminate alone
     (without intros) also works *)
Qed.
}

The reason that @tac{} works in the above example is that @code{1} is a shorthand for @code{S @mark-now["focus"]{O}}, and @code{2 + y} is a shorthand for @code{S (S O) + y} which computes (by definition of @code{+}) to @code{S @mark-now["focus"]{(S y)}}, and the highlighted constructors are mismatched.

The example below fails because there's no immediate mismatched constructors.

@coq-interactive{
Lemma discriminate_does_not_work_here :
  forall (x : nat),
    x = 3 ->
    x = 1 ->
    true = false.
Proof.
  intros.
  @mark["focus"]{discriminate.}
}

One possible way to prove the above is to rewrite @code{x} to @code{3} in @code{x = 1} so that we have the term @code{3 = 1}. After that, @tac{} would now work.

@relevant-tactics[
  @item{Equivalent to @tactic{inversion @mvar{H}.} where @code{@mvar{H}} is a false equality hypothesis. In general @tactic{inversion} is more powerful than @tac{}.}
  @item{Strictly less powerful than @tactic{congruence.}.}
]

@coq-tactic{constructor}

@usage{@tac{}}

@use-when{you want to @emph{solve} or @emph{reduce} the goal because the goal could be constructed by a @emph{constructor}.}

In this example we will use @tac{} to prove that @${4} is even. First we define what exactly is evenness.

@itemlist[
  @item{We define that evenness is a property of a natural number.}
  @item{We define that we can always construct a proof that @${0} is even.}
  @item{Given that we have a proof that @${n} is even, we define that we can also construct a proof that @${2 + n} is even.}
]

This is captured in the following inductive definition of @code{even}:

@highlight['coq]{
Inductive even : nat -> Prop :=
| even_O : even O
| even_S : forall (n : nat), even n -> even (2 + n).
}

Now, we can state our proposition and prove it:

@coq-interactive{
Inductive even : nat -> Prop :=
| even_O : even O
| even_SS : forall (n : nat),
    even n ->
    even (2 + n).

Lemma four_is_even : even 4.
Proof.
  @mark["focus"]{constructor.}
  @mark["focus"]{constructor.}
  @mark["focus"]{constructor.}
Qed.
}

When the goal is @code{even 4}, the first @tac{} finds that the goal can be constructed by using @code{even_SS}. However, it cannot solve the goal because @code{even_SS} requires a proof of @code{even n} (which is @code{even 2} in this case) to construct a proof of @code{even (2 + n)} (which is @code{even 4} in this case). This application of @tac{} thus can only reduce the goal to @code{even 2}.

Similarly, when the goal is @code{even 2}, the second @tac{} finds that the goal can be constructed by using @code{even_SS} and reduces the goal to @code{even 0}.

When the goal is @code{even 0}, the last @tac{} finds that the goal can be constructed by using @code{even_O}. As @code{even_O} doesn't require anything to construct @code{even 0}, so the last @tac{} solves the goal.

@caveat{When there are multiple constructors that can be used to constructed the goal, @tac{} will use the first one, which might not be the one that you want. In that event, the goal might be reduced to a proof state that can't be proven, as we can see below.}

@coq-interactive{
Inductive foo : Prop :=
| FooA : False -> foo
| FooB : foo.

Lemma foo_is_constructible : foo.
Proof.
  @mark["focus"]{constructor.}
  (* Use FooA instead of FooB *)
  (* We get stuck.
     There's no way to prove False! *)
}

The above lemma can be proven by specifying which constructor should be used manually. E.g., by using @tactic{exact FooB}, @tactic{apply FooB}, etc.
@;{we can also use the trick `constructor; fail.` Do we want to write it here?}

@relevant-tactics[
  @item{Equivalent to @tactic{apply @mvar{c}.} where the goal can be constructed by the constructor @code{@mvar{c}}.}
  @item{Equivalent to @tactic{exact @mvar{c}.} if @tac{.} solves the goal.}
]

@coq-tactic{apply}

@usage{@tac{ @mvar{term}}}

@use-when{you want to @emph{solve} the goal because the goal is @code{@mvar{term}}.}


@section{Acknowledgements}

I would like to thank Joe Redmon, whose @link["https://pjreddie.com/coq-tactics/"]{Coq Tactics Index} is the template for this tactic index.