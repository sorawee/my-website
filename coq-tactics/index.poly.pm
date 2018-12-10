#lang pollen

@(require "coq.rkt")

@(define-meta title "Yet Another Coq Tactic Index")
@(define-meta type page)

@title[]

@coq-tactic{reflexivity}

@usage{@tac{}}

@use-when{you want to @emph{solve} the goal which is a @emph{true equality}. I.e., the goal has the form @coq{@mvar{t} = @mvar{u}} and both @coq{@mvar{t}} and @coq{@mvar{u}} are equal syntactically after some computation. The tactic also works if the goal needs an @coq{intros} to be in the form @coq{@mvar{t} = @mvar{u}}.}

In this example we will apply @coq{reflexivity} to prove that for every natural number @${x}, if @${1 = 1} then @${(3 + 2) + x = (2 + 3) + x}.

@coq-interactive{
Lemma complex_math:
  forall x, 1 = 1 -> (3 + 2) + x = (2 + 3) + x.
Proof.
  @mark["focus"]{reflexivity.}
Qed.
}

Note that while Coq can compute concrete values, making it possible to reason that both @coq{(3 + 2) + x} and @coq{(2 + 3) + x} reduce to @coq{5 + x} and therefore are equal, it cannot compute non-concrete values. Hence, @tac{} can't prove the goal that needs non-trivial reasoning, like commutativity of natural numbers that involves non-concrete values.

@coq-interactive{
Lemma commutativity_is_not_trivial:
  forall x, 1 + x = x + 1.
Proof.
  @mark["focus"]{reflexivity.}
}

The above lemma can be solved by performing an @coq{induction} on @coq{x}, or by applying lemmas like @coq{Nat.add_comm}.

@coq-tactic{assumption}

@usage{@tac{}}

@use-when{you want to @emph{solve} the goal which existed already in the context after some computation.}

In this example we will use @tac{} to prove that if @${x = (1 + 2) + y}, then @${x = 3 + y}.

@coq-interactive{
Lemma another_complex_math:
  forall x y, x = (1 + 2) + y -> x = 3 + y.
Proof.
  intros.
  @mark["focus"]{assumption.}
  (* Use "H: x = 1 + 2 + y" *)
Qed.
}

@coq-tactic{discriminate}

@usage{@tac{}}

@use-when{you want to @emph{solve} any goal because the context contains a @emph{false equality} due to @emph{mismatched constructors} after some computation.}

In this example we will use @tac{} to prove that for any natural @${y}, if @${1 = 2 + y}, then @coq{true = false}. Of course, the premise can't be true because the lowest possible value of @${y} is @${0}, and even then @${1 < 2 + 0}.

@coq-interactive{
Lemma mismatch_proves_anything:
  forall y, 1 = 2 + y -> true = false.
Proof.
  intros.
  @mark["focus"]{discriminate.}
  (* Use "H: 1 = 2 + y" *)
Qed.
}

The reason that @tac{} works in the above example is that @coq{1} is a shorthand for @coq{S @mark-now["focus"]{O}}, and @coq{2 + y} is a shorthand for @coq{S (S O) + y} which computes (by definition of @coq{+}) to @coq{S @mark-now["focus"]{(S y)}}, and the highlighted constructors are mismatched.

The example below fails because there's no immediate mismatched constructors.

@coq-interactive{
Lemma discriminate_does_not_work_here:
  forall x, x = 3 -> x = 1 -> true = false.
Proof.
  intros.
  discriminate.
}

One possible way to prove the above is to rewrite @coq{x} to @coq{3} in @coq{x = 1} so that we have the term @coq{3 = 1}. After that, @tac{} would now work.