(** * LevinBudget -- the budget-doubling core of Levin's optimal search.

    Rocq port (statement-level) of [proofs/agda/LevinBudget.agda] (Jones Thm 20.1.2).
    [pow2 k = 2^k], [gsum k = sum_{i=0}^{k} 2^i]; the doubling core is
    [gsum k < 2 * pow2 k], and [overshoot] gives a power of two in [N, 2N].

    Fidelity: statements match the Agda module; proofs are idiomatic Rocq (induction
    + [lia]).  See docs/AGDA_CORRESPONDENCE.md. *)

From Stdlib Require Import Arith Lia.

(** powers of two, by doubling (2^(k+1) = 2^k + 2^k). *)
Fixpoint pow2 (k : nat) : nat :=
  match k with
  | 0 => 1
  | S k' => pow2 k' + pow2 k'
  end.

(** gsum k = sum_{i=0}^{k} 2^i (total budget over phases 0..k). *)
Fixpoint gsum (k : nat) : nat :=
  match k with
  | 0 => 1
  | S k' => pow2 (S k') + gsum k'
  end.

(** exact geometric sum: 1 + gsum k = 2^(k+1). *)
Lemma gsum_eq : forall k, S (gsum k) = pow2 (S k).
Proof.
  induction k as [| k IH]; simpl in *; lia.
Qed.

(** THE DOUBLING CORE: the whole search up to phase k costs < twice the last phase. *)
Lemma gsum_lt_2last : forall k, gsum k < pow2 (S k).
Proof.
  intro k. pose proof (gsum_eq k). lia.
Qed.

(** monotonicity of pow2 and gsum (the operational finish stage is <= the bound stage). *)
Lemma one_le_pow2 : forall n, 1 <= pow2 n.
Proof.
  induction n as [| n IH]; simpl; lia.
Qed.

Lemma pow2_mono : forall m n, m <= n -> pow2 m <= pow2 n.
Proof.
  intros m n H. induction H as [| n Hle IH]; [ lia | simpl; lia ].
Qed.

Lemma one_le_gsum : forall n, 1 <= gsum n.
Proof.
  induction n as [| n IH]; simpl; [ lia |].
  pose proof (one_le_pow2 n). lia.
Qed.

Lemma gsum_mono : forall m n, m <= n -> gsum m <= gsum n.
Proof.
  intros m n H. induction H as [| n Hle IH]; [ lia |].
  simpl. pose proof (one_le_pow2 n). lia.
Qed.

(** pow2 turns addition into multiplication: 2^(m+n) = 2^m * 2^n. *)
Lemma pow2_add : forall m n, pow2 (m + n) = pow2 m * pow2 n.
Proof.
  induction m as [| m IH]; intro n; simpl.
  - lia.
  - rewrite IH. lia.
Qed.

(** For N = S n >= 1, some power of two lies in [N, 2N].  This is the one genuinely
    non-arithmetic fact (a search for the least covering power); by induction on n,
    stepping k up when the current power is first exceeded. *)
Lemma overshoot : forall n, exists k, S n <= pow2 k /\ pow2 k <= S n + S n.
Proof.
  induction n as [| n IH].
  - exists 0. simpl. lia.
  - destruct IH as [k [Hlo Hhi]].
    destruct (le_lt_dec (S (S n)) (pow2 k)) as [Hle | Hlt].
    + (* current power still covers S (S n) *)
      exists k. lia.
    + (* S n = pow2 k exactly; step to k+1 *)
      exists (S k). simpl. lia.
Qed.

(** overshoot for a positive target N >= 1. *)
Lemma overshoot' : forall N, 1 <= N -> exists k, N <= pow2 k /\ pow2 k <= N + N.
Proof.
  intros [| n] H; [ lia |].
  destruct (overshoot n) as [k Hk]. exists k. lia.
Qed.

(** concrete check: gsum 3 = 1+2+4+8 = 15. *)
Example gsum_3 : gsum 3 = 15.
Proof. reflexivity. Qed.
