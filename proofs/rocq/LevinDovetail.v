(** * LevinDovetail -- the dovetail cost skeleton for Levin's optimal search.

    Rocq port (statement-level) of [proofs/agda/LevinDovetail.agda] (Levin B3a).
    Over an abstract phase oracle [done : nat -> bool], [find] scans phases for the first
    success; [time_opt k = gsum k] (batch cost model).  [find_least] proves the search
    finishes exactly at the least success; [find_complete]/[find_finish] give the
    operational finish [kop <= k] used by [LevinComplete]. *)

From Stdlib Require Import Arith Lia.
Require Import LevinBudget.

Section Dovetail.
Variable done : nat -> bool.

(** scan phases i, i+1, ... (fuel-bounded) for the first success. *)
Fixpoint find (f i : nat) : option nat :=
  match f with
  | 0 => None
  | S f' => if done i then Some i else find f' (S i)
  end.

(** cost of phases 0..k: gsum k in the batch model. *)
Definition time_opt (k : nat) : nat := gsum k.

(** core: from a start s <= k where every phase in [s,k) fails and done k holds,
    find (given enough fuel) returns exactly k. *)
Lemma scan : forall f k s,
  s <= k -> done k = true ->
  (forall i, s <= i -> i < k -> done i = false) ->
  S k <= s + f -> find f s = Some k.
Proof.
  induction f as [| f IH]; intros k s Hsk Hk Hfail Hfuel; [ lia |].
  simpl. destruct (Nat.eq_dec s k) as [Heq | Hne].
  - subst. rewrite Hk. reflexivity.
  - assert (Hlt : s < k) by lia.
    rewrite (Hfail s (le_n s) Hlt).
    apply (IH k (S s)); [ lia | exact Hk | | lia ].
    intros i Hi Hik. apply Hfail; [ lia | exact Hik ].
Qed.

Lemma find_least : forall k,
  done k = true -> (forall i, i < k -> done i = false) -> find (S k) 0 = Some k.
Proof.
  intros k Hk Hfail. apply (scan (S k) k 0); [ lia | exact Hk | | lia ].
  intros i _ Hik. apply Hfail; exact Hik.
Qed.

Lemma time_opt_eq : forall k,
  done k = true -> (forall i, i < k -> done i = false) ->
  (exists f, find f 0 = Some k) /\ time_opt k = gsum k.
Proof.
  intros k Hk Hfail. split; [ exists (S k); apply find_least; assumption | reflexivity ].
Qed.

(** converse: from ANY successful phase k, the fuelled search returns the FIRST
    success k', which is <= k and itself a success. *)
Lemma find_complete : forall f i k,
  i <= k -> done k = true -> S k <= i + f ->
  exists k', find f i = Some k' /\ k' <= k /\ done k' = true.
Proof.
  induction f as [| f IH]; intros i k Hik Hk Hfuel; [ lia |].
  simpl. destruct (done i) eqn:Hi.
  - exists i. split; [ reflexivity | split; [ exact Hik | exact Hi ] ].
  - assert (Hlt : i < k) by (destruct (Nat.eq_dec i k); [ subst; congruence | lia ]).
    destruct (IH (S i) k ltac:(lia) Hk ltac:(lia)) as [k' [Hf [Hk'k Hd]]].
    exists k'. split; [ exact Hf | split; [ exact Hk'k | exact Hd ] ].
Qed.

(** the operational finish: given a successful phase k, the search halts at the least
    success kop <= k. *)
Lemma find_finish : forall k,
  done k = true ->
  exists kop, (exists f, find f 0 = Some kop) /\ kop <= k /\ done kop = true.
Proof.
  intros k Hk.
  destruct (find_complete (S k) 0 k (Nat.le_0_l k) Hk ltac:(lia)) as [kop [Hf [Hle Hd]]].
  exists kop. split; [ exists (S k); exact Hf | split; [ exact Hle | exact Hd ] ].
Qed.

End Dovetail.

(** concrete non-vacuous instance: an oracle first succeeding at phase 3. *)
Definition done3 (i : nat) : bool := Nat.leb 3 i.
Example find_done3 : find done3 8 0 = Some 3. Proof. reflexivity. Qed.
Example time_opt_done3 : time_opt 3 = 15. Proof. reflexivity. Qed.
