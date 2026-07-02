(** * LevinOptimal -- Levin's optimality inequality as an abstract meta-theorem.

    Rocq port (statement-level) of [proofs/agda/LevinOptimal.agda] (Jones Thm 20.1.2,
    Layer A).  [finish] shows the dovetail finishes in a phase of cumulative cost < 4N;
    [levin_optimal] assembles it into [gsum k < aq M * S M], [aq = 4 * overhead * 2^idx],
    with the interpreter overhead and the solver index as record fields (discharged for
    the I language in the B-layers).  Proofs via [overshoot']/[gsum_lt_2last] + [nia]. *)

From Stdlib Require Import Arith Lia.
Require Import LevinBudget.

(** the two operational constants as record fields. *)
Record LevinSearch := {
  overhead : nat;   (* a : efficient-interpreter overhead (real steps / simulated step) *)
  idx      : nat;   (* j : the solver's enumeration index *)
  time_q   : nat;   (* T_q = time_q(x) *)
  time_r   : nat    (* T_r = time_r(x, q(x)) *)
}.

Definition Swork (M : LevinSearch) : nat := time_q M + time_r M.
Definition cconst (M : LevinSearch) : nat := overhead M * pow2 (idx M).
Definition aq (M : LevinSearch) : nat := (cconst M + cconst M) + (cconst M + cconst M).

(** the finishing lemma: for a positive target N, the dovetail finishes in some phase k
    whose cumulative cost is < 4N. *)
Lemma finish : forall N, 1 <= N -> exists k, gsum k < (N + N) + (N + N).
Proof.
  intros N HN. destruct (overshoot' N HN) as [k [Hlo Hhi]].
  exists k. pose proof (gsum_lt_2last k) as Hg.
  assert (pow2 (S k) = pow2 k + pow2 k) as Heq by reflexivity.
  lia.
Qed.

(** LEVIN OPTIMALITY: the cumulative cost up to the finishing phase is bounded by
    aq * S -- optimal up to the constant aq = 4 * a * 2^j. *)
Lemma levin_optimal : forall M,
  1 <= cconst M * Swork M -> exists k, gsum k < aq M * Swork M.
Proof.
  intros M Hpos.
  destruct (finish (cconst M * Swork M) Hpos) as [k Hk].
  exists k. unfold aq. nia.
Qed.

(** concrete instance: a=1, j=0, T_q=3, T_r=1  ==>  aq = 4, S = 4, bound aq*S = 16. *)
Definition ex : LevinSearch := {| overhead := 1; idx := 0; time_q := 3; time_r := 1 |}.

Example aq_ex : aq ex = 4. Proof. reflexivity. Qed.

Example levin_ex : exists k, gsum k < 16.
Proof.
  destruct (levin_optimal ex ltac:(cbv; lia)) as [k Hk].
  exists k. cbv [aq ex cconst Swork overhead idx time_q time_r pow2] in Hk. lia.
Qed.
