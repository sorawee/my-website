#lang pollen

@(define-meta title "On Induction")
@(define-meta tags (logic))

In this post, I will write about three kinds of inductions: weak induction, strong induction, and structural induction. I in fact dislike these terms because, as I will show below, they are equivalent. Personally, when I think of induction, I will default to the structural induction. Last semester I took a logic class, and they introduce these terms, and also "formula induction", "proof induction", "term induction", "sequence induction", etc. I was like: *sigh* THEY ARE THE SAME WHY DO YOU NEED DIFFERENT NAMES.

Anyway, for the sake of this article, we will do use these terms for clarity and show the following.

@see-more[]

@/h3{Strong Induction Doesn't Need A Base Case}

Weak Induction states: for any proposition @${P}, @${(P(0) \land \forall x. (P(x) \-> P(x + 1))) \-> \forall x. P(x)}. One must prove both @${P(0)} which is known as @emph{base case} and @${\forall x. (P(x) \-> P(x + 1))} which is known as @emph{inductive case} to show that @${P} holds for every natural number. One common mistake is, sometimes we forget to prove the base case, so we will be able to (incorrectly) prove something clearly wrong like @${\forall x. x > x + 1}. Let's actually try this:

@proof[#:wrong #t]{
  Let @${x} be a natural number. The induction hypothesis states @${x > x + 1}. Our goal is to show that @${x + 1 > x + 2}, but this is just adding 1 to both sides in the inequality from the induction hypothesis. Therefore, @${\forall x. x > x + 1}.
}

This is incorrect because we have not proved the base case that @${0 > 1} which, indeed, is unprovable.

However, in strong induction, we can omit the base case! The traditional strong induction is: for any proposition @${P}, @${(P(0) \land \forall x. ((\forall y < x. P(y)) \-> P(x))) \-> \forall x. P(x)}. But as stated above, in fact just only @${\forall x. ((\forall y < x. P(y)) \-> P(x)) \-> \forall x. P(x)} is enough.

As an example, if you try to use strong induction to prove that @${\forall x. x > x + 1}, we would have:

Let @${x} be a natural number. The induction hypothesis states: @${\forall y < x. y > y + 1} and we want to prove @${\forall x. x > x + 1}. By letting @${y = x - 1}, we obtain that @${x - 1 > x - 1 + 1 = x}. By adding 1 both sides, we obtain @${x > x + 1}. So we conclude that @${\forall x. x > x + 1}...

Wait what!?! We are not supposed to be able to prove this!

Well, there is a subtle error in the above proof. Specifically, when we let @${y = x - 1}. How can we be sure that @${y} is a natural number? The answer is we can't, as when @${x = 0}, @${y = x - 1} is not a natural number! The right way is to do a case analysis here: case @${x = 0} and case @${x > 0}. When @${x = 0}, the induction hypothesis states @${\forall y < 0. y > y + 1} which is a blank statement, and because we can't prove that @${0 > 1}, we can't prove that @${x > x + 1} in case @${x = 0}, so the proof fails!

To be formal, we will derive @${(P(0) \land \forall x. (\forall y < x. P(y) \-> P(x))) \-> \forall x. P(x)} from @${\forall x. ((\forall y < x. P(y)) \-> P(x)) \-> \forall x. P(x)} and vice versa. This shows that both variants of strong induction are equivalent.

@proof{
  @${(\->)} Suppose @${(P(0) \land \forall x. ((\forall y < x. P(y)) \-> P(x))) \-> \forall x. P(x)}. Our goal is to show that @${\forall x. ((\forall y < x. P(y)) \-> P(x)) \-> \forall x. P(x)}. By the deduction theorem, assume @${\forall x. ((\forall y < x. P(y)) \-> P(x))}, we then only need to prove @${\forall x. P(x)}. By applying the hypothesis when @${x = 0}, we obtain @${(\forall y < 0. P(y)) \-> P(0)}. However, @${\forall y < 0. P(y)} is vacuously true, so we obtain @${P(0)}. With both @${P(0)} and @${\forall x. ((\forall y < x. P(y)) \-> P(x))}, we can use the first hypothesis to obtain @${\forall x. P(x)}, finishing the proof.

  @${(\<-)} Suppose @${(\forall x. ((\forall y < x. P(y)) \-> P(x))) \-> \forall x. P(x)}. Our goal is to show that @${(P(0) \land \forall x. ((\forall y < x. P(y)) \-> P(x))) \-> \forall x. P(x)}. By the deduction theorem, assume @${P(0)} and @${\forall x. ((\forall y < x. P(y)) \-> P(x))}, we then only need to prove @${\forall x. P(x)}. But with @${\forall x. ((\forall y < x. P(y)) \-> P(x))}, we can use the first hypothesis to obtain @${\forall x. P(x)}, finishing the proof.
}

@/h3{Equivalence of Weak Induction and Strong Induction}

@highlight['coq]|{
Require Import Omega.

Definition NProp := nat -> Prop.
Theorem weak_eq_to_strong :
  (forall P : NProp, P 0 -> (forall n, P n -> P (S n)) -> forall n, P n) <->
  (forall P : NProp, (forall n, (forall m, (m < n) -> P m) -> P n) -> forall n, P n).
Proof.
  split.
  - intros ind_w P ind_H_s n.
    apply (ind_w (fun k => ((forall x, x <= k -> P x)))) with (n := n); intuition.
    + inversion H; intuition.
      apply ind_H_s; intuition.
      inversion H1.
  - intros; intuition.
    apply H; intuition.
    destruct n0; intuition.
Qed.
}|


Not Yet Written
