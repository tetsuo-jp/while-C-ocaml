(** * NatPairing -- an injective Cantor pairing and its structural inverse.

    Rocq port (statement-level) of [proofs/agda/NatPairing.agda].
    [pair x y = tri (x+y) + x]; [pair_inj] proves it injective (recovering the diagonal
    first), and [unpair] is its structural "diagonal-walk" inverse ([unpair_pair]).
    Proofs are idiomatic Rocq (induction + [lia]). *)

From Stdlib Require Import Arith Lia.

(** triangular numbers  tri n = 0 + 1 + ... + n. *)
Fixpoint tri (n : nat) : nat :=
  match n with
  | 0 => 0
  | S n' => S n' + tri n'
  end.

Definition pair (x y : nat) : nat := tri (x + y) + x.

(** * monotonicity / interval facts for tri. *)

Lemma tri_mono : forall m n, m <= n -> tri m <= tri n.
Proof.
  intros m n H. induction H as [| n Hle IH]; simpl; lia.
Qed.

Lemma n_le_tri : forall n, n <= tri n.
Proof.
  induction n as [| n IH]; simpl; lia.
Qed.

Lemma tri_bound : forall a d, a <= d -> tri d + a < tri (S d).
Proof.
  intros a d H. simpl. lia.
Qed.

(** the diagonal a+b is determined by the pair value. *)
Lemma diag_eq : forall a d c e,
  a <= d -> c <= e -> tri d + a = tri e + c -> d = e.
Proof.
  intros a d c e Ha Hc Heq.
  destruct (lt_eq_lt_dec d e) as [[Hlt | Hde] | Hgt]; auto.
  - exfalso. pose proof (tri_bound a d Ha). pose proof (tri_mono (S d) e Hlt). lia.
  - exfalso. pose proof (tri_bound c e Hc). pose proof (tri_mono (S e) d Hgt). lia.
Qed.

(** * injectivity. *)

Lemma pair_inj : forall a b c d, pair a b = pair c d -> a = c /\ b = d.
Proof.
  intros a b c d Heq. unfold pair in Heq.
  assert (Hdiag : a + b = c + d).
  { apply (diag_eq a (a + b) c (c + d)); [ lia | lia | exact Heq ]. }
  rewrite <- Hdiag in Heq. split; lia.
Qed.

(** * the structural inverse (diagonal-walk successor recursions). *)

Lemma pair_S_l : forall x y, pair (S x) y = S (pair x (S y)).
Proof.
  intros x y. unfold pair.
  replace (S x + y) with (S (x + y)) by lia.
  replace (x + S y) with (S (x + y)) by lia.
  lia.
Qed.

Lemma pair_0_S : forall x, pair 0 (S x) = S (pair x 0).
Proof.
  intro x. unfold pair.
  replace (0 + S x) with (S x) by lia.
  replace (x + 0) with x by lia.
  simpl. lia.
Qed.

Definition unpair_step (p : nat * nat) : nat * nat :=
  match p with
  | (x, 0)   => (0, S x)
  | (x, S y) => (S x, y)
  end.

Fixpoint unpair (n : nat) : nat * nat :=
  match n with
  | 0    => (0, 0)
  | S n' => unpair_step (unpair n')
  end.

(** pair o unpair = id. *)
Lemma unpair_correct : forall n, pair (fst (unpair n)) (snd (unpair n)) = n.
Proof.
  induction n as [| n IH]; [ reflexivity |].
  simpl. destruct (unpair n) as [x y]. simpl in IH. destruct y as [| y'].
  - simpl. rewrite pair_0_S. rewrite IH. reflexivity.
  - simpl. rewrite pair_S_l. rewrite IH. reflexivity.
Qed.

(** hence unpair o pair = id. *)
Lemma unpair_pair : forall x y, unpair (pair x y) = (x, y).
Proof.
  intros x y.
  pose proof (unpair_correct (pair x y)) as Hc.
  destruct (unpair (pair x y)) as [x' y']. simpl in Hc.
  destruct (pair_inj x' y' x y Hc) as [Hx Hy]. subst. reflexivity.
Qed.

(** * component bounds (for well-founded inversion of vnat in ProgramDecode). *)

Lemma pair_fst_le : forall x y, x <= pair x y.
Proof. intros x y. unfold pair. lia. Qed.

Lemma pair_snd_le : forall x y, y <= pair x y.
Proof.
  intros x y. unfold pair. pose proof (n_le_tri (x + y)). lia.
Qed.

(** concrete checks. *)
Example unpair_2 : unpair 2 = (1, 0). Proof. reflexivity. Qed.
Example pair_inj_check : pair 3 5 = 39. Proof. reflexivity. Qed.
