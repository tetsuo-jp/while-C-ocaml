{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 ASSEMBLY, the `'while` case (standalone, on a straight-line body).
-- The LAST fragment of the whole-spec congruence `[cogen(p,s)](d)=[p]((s,d))`.
--
-- A dynamic `while g c` is specialised against a fixed loop av-store σloop -- the
-- GENERALISATION of the loop-carried set.  Two parts, mirroring the `'if` merge:
--
--   * ENTRY (`spec-while-entry`): generalise the assigned set (`dynList (assigned c)`)
--       so the entry stores become consistent with σloop -- a direct corollary of
--       `dynlist-sound` (emit one residual fixup per loop-carried var).
--   * LOOP (`spec-while-sound`): provided σloop is a FIXPOINT of the body
--       specialiser (`proj₂ (spec c σloop) ≡ σloop` -- what iterating the body's
--       BTA to a fixpoint establishes in Cogen.ml, so the residual loop folds),
--       the body maps consistent (source,residual) stores to consistent ones
--       (this is `SpecAssembly.spec-sound`, the body congruence, rewritten along
--       the fixpoint), hence by `WhileCase.while-store-cong` (= MemoFold.run-inv)
--       the whole loop preserves consistency: if it exits, the final stores agree.
--
-- This is the assembly step: it TIES the proven body congruence (`spec-sound`)
-- to the proven loop kernel (`WhileCase`/`MemoFold.run-inv`) through the loop
-- generalisation invariant.  With `SpecAssembly` (straight-line) and
-- `SpecAssemblyIf` (`'if`), the whole-spec congruence now covers every command
-- form: skip / asgn / semi / if / while.  Referenced by docs/AGDA_CORRESPONDENCE.md.
------------------------------------------------------------------------

module SpecAssemblyWhile where

open import Data.Nat using (ℕ)
open import Data.Nat.Properties using (_≟_)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

open import AVExprSound using (Val; Expr; var; tl; AV; Dv)
open import SpecExprStore using (Consistent)
open import SpecCmdStore
  using (SelfRef; selfref-dep; dynList; dynListRS; dynlist-sound; module WhileCase)
open import SpecAssembly using (Cmd; asgn; cexec; spec; spec-sound; assigned)
open import SpecAssemblyIf using (consistent-cong)

----------------------------------------------------------------------
-- LOOP ENTRY.  Generalising the loop-carried set (the assigned vars) makes the
-- entry stores consistent with σloop = dynList (assigned c) σ, and keeps σloop
-- self-referential -- the precondition the loop congruence below needs.  A direct
-- corollary of `dynlist-sound` (the shared generalisation step).
spec-while-entry : ∀ c σ ss rs → Consistent σ ss rs → SelfRef σ
                 → Consistent (dynList (assigned c) σ) ss (dynListRS (assigned c) σ rs)
                 × SelfRef (dynList (assigned c) σ)
spec-while-entry c σ ss rs cons self = dynlist-sound (assigned c) σ ss rs cons self

----------------------------------------------------------------------
-- THE LOOP CASE.  σloop is a fixpoint of the body specialiser (the loop
-- generalisation invariant: re-specialising the body under σloop returns σloop,
-- so the residual `while` folds).  Then the loop preserves store consistency.
--
-- `let open WhileCase … in …` instantiates the proven loop kernel at this g /
-- body / σloop; the result type is its `MaybeInv Inv (run pstep n …)` -- i.e. if
-- the fuel-bounded loop exits, the final (source, residual) stores are consistent
-- with σloop.  The body-step hypothesis is `spec-sound` (the body congruence)
-- rewritten along the fixpoint via `consistent-cong`.
spec-while-sound :
  ∀ g c σloop → (∀ j → proj₂ (spec c σloop) j ≡ σloop j) → SelfRef σloop
  → ∀ n ss rs → Consistent σloop ss rs
  → let open WhileCase g σloop (cexec c) (cexec (proj₁ (spec c σloop)))
    in MaybeInv Inv (run pstep n (ss , rs))
spec-while-sound g c σloop fix self n ss rs cons =
  while-store-cong
    (λ ss′ rs′ c′ → consistent-cong (proj₂ (spec c σloop)) σloop fix
                       (proj₁ (spec-sound c σloop ss′ rs′ c′ self)))
    n ss rs cons
  where open WhileCase g σloop (cexec c) (cexec (proj₁ (spec c σloop)))

----------------------------------------------------------------------
-- NON-VACUITY: the fixpoint hypothesis of `spec-while-sound` IS satisfiable on a
-- real loop, with NOTHING assumed.  Take the decrementing loop
--   while x₀ do x₀ := tl x₀
-- and the all-dynamic loop store σ⋆ = λk. Dv (var k).  The body's RHS READS the
-- dynamic var x₀, so the assignment EMITS and the slot stays `Dv (var 0)` -- hence
-- σ⋆ is a genuine generalisation fixpoint (`fix⋆`, by computation).  (A static
-- RHS like `x₀ := 'nil` would FOLD the slot back to `Sv vnil`, breaking the
-- fixpoint -- which is exactly why the general statement needs the hypothesis.)
-- So `spec-while-sound` fires here with no open assumption: a fully closed,
-- end-to-end `'while` soundness instance.
c⋆ : Cmd
c⋆ = asgn 0 (tl (var 0))

σ⋆ : ℕ → AV
σ⋆ k = Dv (var k)

fix⋆ : ∀ j → proj₂ (spec c⋆ σ⋆) j ≡ σ⋆ j
fix⋆ j with 0 ≟ j
... | yes refl = refl
... | no  _    = refl

self⋆ : SelfRef σ⋆
self⋆ = selfref-dep

example-while-sound :
  ∀ n ss rs → Consistent σ⋆ ss rs
  → let open WhileCase (var 0) σ⋆ (cexec c⋆) (cexec (proj₁ (spec c⋆ σ⋆)))
    in MaybeInv Inv (run pstep n (ss , rs))
example-while-sound = spec-while-sound (var 0) c⋆ σ⋆ fix⋆ self⋆
