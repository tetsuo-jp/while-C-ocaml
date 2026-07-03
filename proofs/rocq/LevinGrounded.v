(** * LevinGrounded -- Levin optimality with the operational constants grounded.

    Rocq port (statement-level) of [proofs/agda/LevinGrounded.agda] (Levin Phase 4).
    Feeds the interpreter overhead [a = 73] and the solver index [index q] into Layer A's
    abstract [LevinSearch].  NOTE: in the Agda original [overhead := IntEfficient.a] is a
    THEOREM (int-efficient); here [a] is the stated constant [73], whose justification is
    the (as-yet unported) interpreter-cost stack -- everything else is proved. *)

From Stdlib Require Import Arith Lia.
Require Import LevinBudget.
Require Import LevinOptimal.
Require Import UniversalInterp.
Require Import ProgramEnum.

(** the linear-overhead constant a = 73 (= IntEfficient.a in the Agda development). *)
Definition a_overhead : nat := 73.

Definition search (j Tq Tr : nat) : LevinSearch :=
  {| overhead := a_overhead; idx := j; time_q := Tq; time_r := Tr |}.

Example overhead_is_73 : forall j Tq Tr, overhead (search j Tq Tr) = 73.
Proof. reflexivity. Qed.

Example aq_search : aq (search 0 3 1) = 292.
Proof. reflexivity. Qed.

(** Levin optimality with a = 73 supplied: gsum k < 4*73*2^j*(Tq+Tr). *)
Lemma levin_grounded : forall j Tq Tr,
  1 <= cconst (search j Tq Tr) * Swork (search j Tq Tr) ->
  exists k, gsum k < aq (search j Tq Tr) * Swork (search j Tq Tr).
Proof. intros j Tq Tr H. apply levin_optimal. exact H. Qed.

(** for a concrete solver program q, its index is index q (B2a). *)
Definition search_solver (q : Exp) (Tq Tr : nat) : LevinSearch := search (index q) Tq Tr.

Example overhead_solver : forall q Tq Tr, overhead (search_solver q Tq Tr) = 73.
Proof. reflexivity. Qed.

Example idx_solver : forall q Tq Tr, idx (search_solver q Tq Tr) = index q.
Proof. reflexivity. Qed.

Lemma levin_grounded_solver : forall q Tq Tr,
  1 <= cconst (search_solver q Tq Tr) * Swork (search_solver q Tq Tr) ->
  exists k, gsum k < aq (search_solver q Tq Tr) * Swork (search_solver q Tq Tr).
Proof. intros q Tq Tr H. apply levin_optimal. exact H. Qed.

(** concrete: index 0, Tq=3, Tr=1  ==>  c=73, S=4, aq*S=1168. *)
Example levin_ex : exists k, gsum k < 1168.
Proof.
  destruct (levin_grounded 0 3 1 ltac:(cbv; lia)) as [k Hk].
  exists k. cbv [aq cconst Swork search overhead idx time_q time_r a_overhead pow2] in Hk. lia.
Qed.
