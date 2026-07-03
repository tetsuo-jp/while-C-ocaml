(** * SintCostR -- Levin Phase 5: COST dispatch equations for the self-interpreter
    [sint] over the step-counting semantics [evalT] (StepCount.v).  Ports the Agda
    [SintCost] twin; ALL seven per-node dispatch constants are here and EQUAL to Agda:
      var 8 / knil 12 / khd 39 / ktl 48 / kcons 45 / kifnil 73 / call 64  (a = 73 = max).
    [IntEfficientR] feeds these clean corollaries to the [Steps]-induction for a = 73.

    Technique = SimF.v (symmetric-fuel dispatch equations at the SAME fuel offsets,
    closed by [vm_compute] -- NOT kernel [reflexivity], which is exponential), but
    now each equation also tracks the unit-cost step count.

    KEY DISCOVERY (shape): the naive RHS [Some (hd (fst r), 39 + snd r)] does NOT
    close by vm_compute.  The LHS normal form keeps the interpreter's return frames
    as a CASCADE of stuck matches around the frozen recursive sub-run -- one bump
    layer per evalT frame of sint's dispatch spine -- and vm_compute cannot collapse
    a match on a stuck match.  So the equation must be stated with the SAME cascade
    on the RHS ([bumpN]/[hd1]/[tl1] below).  The clean [K + t] corollary then follows
    by rewriting the hypothesis into the cascade (the bumps collapse once the inner
    run is a concrete [Some (v, t)]).

    Found constants (probe: Eval vm_compute at concrete instances, p := var, env := Vnil):
      - base var  : 8            (fuel 20 suffices)   [Agda twin: 8  -- equal]
      - base knil : 12           (fuel 20 suffices)   [Agda twin: 12 -- equal]
      - khd  : K = 39 at 12/6, cascade (inside out)  +12, hd.+1, +8, +7, +6, +5
               (5+6+7+8+1+12 = 39)                    [Agda twin: 39 -- equal]
      - ktl  : K = 48 at 13/6, cascade (inside out)  +12, tl.+1, +9, +8, +7, +6, +5
               (5+6+7+8+9+1+12 = 48)                  [Agda twin: 48 -- equal]
    Raw probe data (fuel, total): khd var @25 -> 47, ktl var @26 -> 56,
    inner var @19 -> 8; 47-8 = 39, 56-8 = 48. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import StepCount.

(** One interpreter return frame: cost bump by a concrete [k]. *)
Definition bumpN (k : nat) (o : option (V * nat)) : option (V * nat) :=
  match o with Some r => Some (fst r, k + snd r) | None => None end.

(** Return frame that also takes the head / tail of the value (cost +1). *)
Definition hd1 (o : option (V * nat)) : option (V * nat) :=
  match o with Some r => Some (hd (fst r), S (snd r)) | None => None end.
Definition tl1 (o : option (V * nat)) : option (V * nat) :=
  match o with Some r => Some (tl (fst r), S (snd r)) | None => None end.

(** base cost: variable lookup costs 8 sint-steps *)
Lemma cost_var : forall p env,
  evalT sint 20 sint (Vcons (enc p) (Vcons (enc var) env)) = Some (env, 8).
Proof. intros. vm_compute. reflexivity. Qed.

(** base cost: nil literal costs 12 sint-steps *)
Lemma cost_knil : forall p env,
  evalT sint 20 sint (Vcons (enc p) (Vcons (enc knil) env)) = Some (Vnil, 12).
Proof. intros. vm_compute. reflexivity. Qed.

(** khd cost dispatch (fuel offsets 12/6 as SimF.disp_khd_eq); frame cascade sums to 39. *)
Lemma cost_khd_eq : forall m p e env,
  evalT sint (12 + m) sint (Vcons (enc p) (Vcons (enc (khd e)) env)) =
  bumpN 5 (bumpN 6 (bumpN 7 (bumpN 8 (hd1 (bumpN 12
    (evalT sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env)))))))).
Proof. intros. vm_compute. reflexivity. Qed.

(** clean corollary: interpreting [khd e] costs (cost of interpreting e) + 39 *)
Lemma cost_khd : forall m p e env v t,
  evalT sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env)) = Some (v, t) ->
  evalT sint (12 + m) sint (Vcons (enc p) (Vcons (enc (khd e)) env)) = Some (hd v, 39 + t).
Proof. intros. rewrite cost_khd_eq, H. reflexivity. Qed.

(** ktl cost dispatch (fuel offsets 13/6 as SimF.disp_ktl_eq); frame cascade sums to 48. *)
Lemma cost_ktl_eq : forall m p e env,
  evalT sint (13 + m) sint (Vcons (enc p) (Vcons (enc (ktl e)) env)) =
  bumpN 5 (bumpN 6 (bumpN 7 (bumpN 8 (bumpN 9 (tl1 (bumpN 12
    (evalT sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))))))))).
Proof. intros. vm_compute. reflexivity. Qed.

(** clean corollary: interpreting [ktl e] costs (cost of interpreting e) + 48 *)
Lemma cost_ktl : forall m p e env v t,
  evalT sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env)) = Some (v, t) ->
  evalT sint (13 + m) sint (Vcons (enc p) (Vcons (enc (ktl e)) env)) = Some (tl v, 48 + t).
Proof. intros. rewrite cost_ktl_eq, H. reflexivity. Qed.

(** ** kcons cost dispatch (fuel offsets 12/7 as SimF.disp_kcons_eq); K = 45.
    Cascade: 3 return frames (5+6+7) + two sub-call frames (13+13) + kcons node (S).
    (5+6+7+13+13+1 = 45)                              [Agda twin: 45 -- equal] *)
Lemma cost_kcons_eq : forall m p a b env,
  evalT sint (12 + m) sint (Vcons (enc p) (Vcons (enc (kcons a b)) env)) =
  bumpN 5 (bumpN 6 (bumpN 7 (
    match bumpN 13 (evalT sint (7 + m) sint (Vcons (enc p) (Vcons (enc a) env))) with
    | None => None
    | Some ra => match bumpN 13 (evalT sint (7 + m) sint (Vcons (enc p) (Vcons (enc b) env))) with
                 | None => None
                 | Some rb => Some (Vcons (fst ra) (fst rb), S (snd ra + snd rb))
                 end
    end))).
Proof. intros. vm_compute. reflexivity. Qed.

(** clean corollary: interpreting [kcons a b] costs (cost a) + (cost b) + 45 *)
Lemma cost_kcons : forall m p a b env va ta vb tb,
  evalT sint (7 + m) sint (Vcons (enc p) (Vcons (enc a) env)) = Some (va, ta) ->
  evalT sint (7 + m) sint (Vcons (enc p) (Vcons (enc b) env)) = Some (vb, tb) ->
  evalT sint (12 + m) sint (Vcons (enc p) (Vcons (enc (kcons a b)) env)) = Some (Vcons va vb, 45 + (ta + tb)).
Proof. intros. rewrite cost_kcons_eq, H, H0. cbn. f_equal. f_equal. lia. Qed.

(** ** kifnil cost dispatch (fuel offsets 16/8 as SimF.disp_kifnil_eq); K = 73 (= a).
    Cascade: 6 return frames (5+6+7+8+9+10) + guard frame (13) + branch frame (14) +
    node (S).  (5+6+7+8+9+10+13+14+1 = 73)            [Agda twin: 73 -- equal] *)
Lemma cost_kifnil_eq : forall m p c t e env,
  evalT sint (16 + m) sint (Vcons (enc p) (Vcons (enc (kifnil c t e)) env)) =
  bumpN 5 (bumpN 6 (bumpN 7 (bumpN 8 (bumpN 9 (bumpN 10 (
    match bumpN 13 (evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc c) env))) with
    | None => None
    | Some rc =>
        ifnil (fst rc)
          (match bumpN 14 (evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc t) env))) with
           | None => None
           | Some rt => Some (fst rt, S (snd rc + snd rt))
           end)
          (match bumpN 14 (evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc e) env))) with
           | None => None
           | Some re => Some (fst re, S (snd rc + snd re))
           end)
    end)))))).
Proof. intros. vm_compute. reflexivity. Qed.

(** clean corollary, then-branch: guard is Vnil, take [t] *)
Lemma cost_kifnil_then : forall m p c t e env tg vt tt,
  evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc c) env)) = Some (Vnil, tg) ->
  evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc t) env)) = Some (vt, tt) ->
  evalT sint (16 + m) sint (Vcons (enc p) (Vcons (enc (kifnil c t e)) env)) = Some (vt, 73 + (tg + tt)).
Proof. intros. rewrite cost_kifnil_eq, H, H0. cbn. f_equal. f_equal. lia. Qed.

(** clean corollary, else-branch: guard is a cons, take [e] *)
Lemma cost_kifnil_else : forall m p c t e env x y tg ve te,
  evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc c) env)) = Some (Vcons x y, tg) ->
  evalT sint (8 + m) sint (Vcons (enc p) (Vcons (enc e) env)) = Some (ve, te) ->
  evalT sint (16 + m) sint (Vcons (enc p) (Vcons (enc (kifnil c t e)) env)) = Some (ve, 73 + (tg + te)).
Proof. intros. rewrite cost_kifnil_eq, H, H0. cbn. f_equal. f_equal. lia. Qed.

(** ** call cost dispatch (fuel offsets 20/10/13 as DispCall.disp_call_eq); K = 64.
    Nested double interpretation (argument then apply): inner run (fuel 10) wrapped by a
    +12 dispatch bump, two argenv-build wraps (+3 each, consing [enc p]), the outer apply
    run (fuel 13) on [(enc p . (enc p . arg))], the call node (S), and 6 return frames
    (5..10 = 45).  (45 + 12 + 3 + 3 + 1 = 64)         [Agda twin: 64 -- equal] *)
Lemma cost_call_eq : forall m p e env,
  evalT sint (20 + m) sint (Vcons (enc p) (Vcons (enc (call e)) env)) =
  bumpN 5 (bumpN 6 (bumpN 7 (bumpN 8 (bumpN 9 (bumpN 10 (
    match (match (match bumpN 12 (evalT sint (10 + m) sint (Vcons (enc p) (Vcons (enc e) env))) with
                  | None => None
                  | Some rb' => Some (Vcons (enc p) (fst rb'), 3 + snd rb')
                  end)
           with
           | None => None
           | Some rb => Some (Vcons (enc p) (fst rb), 3 + snd rb)
           end)
    with
    | None => None
    | Some rargenv =>
        match evalT sint (13 + m) sint (fst rargenv) with
        | None => None
        | Some rv => Some (fst rv, S (snd rargenv + snd rv))
        end
    end)))))).
Proof. intros. vm_compute. reflexivity. Qed.

(** clean corollary: if [e] self-interprets to [arg] (cost ta) and applying [p] to
    [arg] self-interprets to [v] (cost tv), then [call e] costs ta + tv + 64. *)
Lemma cost_call : forall m p e env arg ta v tv,
  evalT sint (10 + m) sint (Vcons (enc p) (Vcons (enc e) env)) = Some (arg, ta) ->
  evalT sint (13 + m) sint (Vcons (enc p) (Vcons (enc p) arg)) = Some (v, tv) ->
  evalT sint (20 + m) sint (Vcons (enc p) (Vcons (enc (call e)) env)) = Some (v, 64 + (ta + tv)).
Proof.
  intros m p e env arg ta v tv H H0.
  rewrite cost_call_eq, H. cbn -[Nat.add evalT enc]. rewrite H0. cbn -[Nat.add]. f_equal.
Qed.
