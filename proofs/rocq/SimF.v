(** * SimF -- self-interpreter correctness, recursive dispatch (Phase 2+, SIMF-PLAN.md).

    The make-or-break recursive dispatch, cracked.  KEY: state the dispatch as a
    SYMMETRIC-fuel equation where the outer fuel = inner + (dispatch cost) and the inner
    fuel keeps enough CONCRETE steps to run the tag-walk guards; then both sides share the
    recursive sub-run at the SAME fuel, so [reflexivity] closes it.  The per-constructor
    lemma then lifts the sub-run hypothesis by [eval_mono_le] and rewrites. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import SelfInterpCorrect.

Definition omap (f : V -> V) (o : option V) : option V :=
  match o with Some w => Some (f w) | None => None end.

(** khd dispatch equation (by kernel lazy reflexivity): outer = 12+m, recursive = 6+m
    (both sides run the sub-triple at fuel 6+m; 6 concrete steps feed the tag-3 guards). *)
Lemma disp_khd_eq : forall m p e env,
  eval sint (12 + m) sint (Vcons (enc p) (Vcons (enc (khd e)) env)) =
  omap hd (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. reflexivity. Qed.

(** the khd dispatch lemma: interpreting (khd e) = hd of interpreting e. *)
Lemma disp_khd : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (12 + F) p (khd e) env = Some (hd v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v).
  { apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]. }
  rewrite (disp_khd_eq F p e env). rewrite H6. reflexivity.
Qed.

(** ktl dispatch (tag 4, one deeper than khd): outer = 13+m, recursive = 6+m. *)
Lemma disp_ktl_eq : forall m p e env,
  eval sint (13 + m) sint (Vcons (enc p) (Vcons (enc (ktl e)) env)) =
  omap tl (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. reflexivity. Qed.

Lemma disp_ktl : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (13 + F) p (ktl e) env = Some (tl v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v).
  { apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]. }
  rewrite (disp_ktl_eq F p e env). rewrite H6. reflexivity.
Qed.

(** PROVEN: the single-sub recursive dispatches (khd/ktl) -- the make-or-break, showing
    the symmetric-fuel equation + eval_mono_le technique closes the recursive dispatch
    that reflexivity/cbn alone cannot (SIMF-PLAN.md Phase 2).

    REMAINING (same technique, subtler offsets): the two-sub [disp_kcons] and the
    guard+branch [disp_kifnil].  The naive fuel count (outer = branch-depth + node-count)
    predicts the offsets for khd/ktl but not yet for kcons -- reflexivity genuinely fails
    at the predicted offsets (not a timeout), so the kcons/kifnil sub-run fuels need
    empirical calibration (their sub-requests use deeper accessors, [ktl argsE] /
    [khd (ktl argsE)], than khd's [argsE]).  Then Phase 3 (call-free [sim] induction) and
    Phase 4 (the [call] case) follow. *)
