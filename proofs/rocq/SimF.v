(** * SimF -- self-interpreter correctness, recursive dispatch (Phase 2, SIMF-PLAN.md).

    The make-or-break recursive dispatch, CRACKED for the single-sub constructors.  KEY:
    state the dispatch as a SYMMETRIC-fuel equation where the outer fuel = inner +
    (dispatch cost) and the inner fuel keeps enough CONCRETE steps to run the tag-walk
    guards; then both sides share the recursive sub-run at the SAME fuel, so [reflexivity]
    closes what [reflexivity]/[cbn] on the single-sided goal cannot.  The per-constructor
    lemma then lifts the sub-run hypothesis by [eval_mono_le].

    PERFORMANCE NOTE / DEFERRED WORK.  The SINGLE-sub equations (khd/ktl) close in ~5s.
    The MULTI-sub equations blow up under kernel [reflexivity] (kcons_eq ~100s, kifnil_eq
    >300s): reducing the giant [sint] symbolically with two/three recursive sub-runs is
    exponential.  So kcons/kifnil dispatch (and hence the [sim] induction) are DEFERRED to
    a faster proof route -- manual [ev_*] stepping + the [SelfInterpCorrect] guard/accessor
    lemmas, which reduces only the dispatch spine and keeps each sub-run opaque, avoiding
    the blow-up.  Offsets already found empirically: khd 12/6, ktl 13/6, kcons 12/7,
    kifnil 16/8 (all verified in isolation; only kcons/kifnil are too slow to keep here). *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import SelfInterpCorrect.

Definition omap (f : V -> V) (o : option V) : option V :=
  match o with Some w => Some (f w) | None => None end.

(** khd dispatch equation (kernel lazy reflexivity, ~5s): outer = 12+m, recursive = 6+m. *)
Lemma disp_khd_eq : forall m p e env,
  eval sint (12 + m) sint (Vcons (enc p) (Vcons (enc (khd e)) env)) =
  omap hd (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. reflexivity. Qed.

(** interpreting (khd e) = hd of interpreting e. *)
Lemma disp_khd : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (12 + F) p (khd e) env = Some (hd v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v)
    by (apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]).
  rewrite (disp_khd_eq F p e env). rewrite H6. reflexivity.
Qed.

(** ktl dispatch (tag 4): outer = 13+m, recursive = 6+m. *)
Lemma disp_ktl_eq : forall m p e env,
  eval sint (13 + m) sint (Vcons (enc p) (Vcons (enc (ktl e)) env)) =
  omap tl (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. reflexivity. Qed.

Lemma disp_ktl : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (13 + F) p (ktl e) env = Some (tl v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v)
    by (apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]).
  rewrite (disp_ktl_eq F p e env). rewrite H6. reflexivity.
Qed.

(** call-free predicate + sub-run fuel monotonicity (for the deferred [sim] induction). *)
Fixpoint call_free (e : Exp) : Prop :=
  match e with
  | var => True | knil => True
  | khd e' => call_free e' | ktl e' => call_free e'
  | kcons a b => call_free a /\ call_free b
  | kifnil c t e' => call_free c /\ call_free t /\ call_free e'
  | call _ => False
  end.

Lemma selfrun_mono : forall F F' p e env v,
  F <= F' -> selfrun F p e env = Some v -> selfrun F' p e env = Some v.
Proof.
  unfold selfrun. intros F F' p e env v Hle H. apply (eval_mono_le sint F F'); assumption.
Qed.
