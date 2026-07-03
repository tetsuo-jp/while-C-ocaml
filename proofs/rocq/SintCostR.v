(** * SintCostR (draft) -- Levin Phase 5 pilot: COST dispatch equations for the
    self-interpreter [sint] over the step-counting semantics [evalT] (StepCount.v).

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

From Stdlib Require Import Arith.
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
