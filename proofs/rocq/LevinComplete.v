(** * LevinComplete -- end-to-end operational Levin optimality.

    Rocq port (statement-level) of [proofs/agda/LevinComplete.agda] (Levin B3c).
    Combines [LevinDovetail] (operational finish) with [LevinGrounded]/[LevinOptimal]
    (the bound) at the finishing phase: [levin_operational] takes only [done k = true] +
    the Layer-A bound at [k]; [find_finish] locates the least success [kop <= k], and
    [gsum_mono] carries the bound down to [gsum kop < aq * Swork]. *)

From Stdlib Require Import Arith Lia.
Require Import LevinBudget.
Require Import LevinOptimal.
Require Import LevinGrounded.
Require Import LevinDovetail.

Section Complete.
Variable done : nat -> bool.

(** at the finishing phase k (least success), the operational search halts with real
    cost gsum k, which is Levin-optimal. *)
Lemma levin_complete : forall j Tq Tr k,
  gsum k < aq (search j Tq Tr) * Swork (search j Tq Tr) ->
  done k = true -> (forall i, i < k -> done i = false) ->
  (exists f, find done f 0 = Some k) /\ (time_opt k = gsum k) /\
  (gsum k < aq (search j Tq Tr) * Swork (search j Tq Tr)).
Proof.
  intros j Tq Tr k Hbnd Hk Hfail. split; [| split ].
  - exists (S k). apply find_least; [ exact Hk | exact Hfail ].
  - reflexivity.
  - exact Hbnd.
Qed.

(** the general form (B3b-ready): the operational finish kop need only be <= the bound
    phase k (a faster program may verify earlier -- still optimal, gsum kop <= gsum k). *)
Lemma levin_operational : forall j Tq Tr k,
  gsum k < aq (search j Tq Tr) * Swork (search j Tq Tr) ->
  done k = true ->
  exists kop, (exists f, find done f 0 = Some kop) /\ (time_opt kop = gsum kop) /\
    (gsum kop < aq (search j Tq Tr) * Swork (search j Tq Tr)).
Proof.
  intros j Tq Tr k Hbnd Hk.
  destruct (find_finish done k Hk) as [kop [Hf [Hle Hd]]].
  exists kop. split; [ exact Hf | split ].
  - reflexivity.
  - apply Nat.le_lt_trans with (m := gsum k); [ apply gsum_mono; exact Hle | exact Hbnd ].
Qed.

End Complete.

(** concrete: solver j=0,Tq=3,Tr=1 (aq*S=1168); an oracle first succeeding at phase 9
    (gsum 9 = 1023 < 1168) fires the whole chain. *)
Example levin_op_ex :
  exists kop,
    (exists f, find (fun i => Nat.leb 9 i) f 0 = Some kop) /\
    (time_opt kop = gsum kop) /\
    (gsum kop < aq (search 0 3 1) * Swork (search 0 3 1)).
Proof.
  apply (levin_operational (fun i => Nat.leb 9 i) 0 3 1 9).
  - assert (E1 : gsum 9 = 1023) by reflexivity.
    assert (E2 : aq (search 0 3 1) * Swork (search 0 3 1) = 1168) by reflexivity.
    rewrite E1, E2. lia.
  - reflexivity.
Qed.
