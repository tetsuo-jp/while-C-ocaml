{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- ONE VARIABLE IS ENOUGH: the I language (one-variable WHILE/Exp) is universal.
--
-- `UniversalInterp.Exp` IS Jones's I language: a SINGLE variable `var`, the
-- unbounded binary-tree data domain `V`, conditionals, and `call` (recursion).
-- "Only one variable" is no restriction on power, for the classic reason: the
-- single variable holds an unbounded TREE, so it can pack an UNBOUNDED number of
-- independent registers.  This module machine-checks that packing.
--
--   * a store is a list packed in the one value; `getR i` / `setR i` read/write
--     register `i` (any ℕ);
--   * `get-set-eq` and `get-set-neq` are the array axioms: writing register `j`
--     leaves every other register `i ≠ j` unchanged, and reads back at `j`.
-- So the one variable simulates ℕ-many independent registers.
--
-- The Turing-completeness of the one-variable language then rests on three
-- machine-checked facts together:
--   (1) one variable = unboundedly many independent registers   [THIS module];
--   (2) `call` gives unbounded recursion / iteration            [UniversalInterp];
--   (3) a UNIVERSAL one-variable program exists -- `SelfInterp.sint` interprets
--       every encoded program and uses only `var`               [SelfInterp,
--       SelfInterpRun].
-- (1)+(2) show one variable matches multi-variable WHILE (Jones's universal
-- model); (3) exhibits a concrete universal program written with one variable.
--
-- See docs/I-LANGUAGE.md.  Referenced by docs/AGDA_CORRESPONDENCE.md.
------------------------------------------------------------------------

module OneVarTuring where

open import Data.Nat using (ℕ; zero; suc)
open import Data.Empty using (⊥-elim)
open import Relation.Nullary using (¬_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import UniversalInterp using (V; ⟨⟩; _∙_; hd; tl)

------------------------------------------------------------------------
-- Registers packed into one value (a list); register i lives at depth i.
------------------------------------------------------------------------

getR : ℕ → V → V
getR zero    s = hd s
getR (suc i) s = getR i (tl s)

setR : ℕ → V → V → V
setR zero    v s = v ∙ tl s
setR (suc i) v s = hd s ∙ setR i v (tl s)

------------------------------------------------------------------------
-- The array axioms: ℕ-many independent registers in the single variable.
------------------------------------------------------------------------

-- read back what you just wrote.
get-set-eq : ∀ i v s → getR i (setR i v s) ≡ v
get-set-eq zero    v s = refl
get-set-eq (suc i) v s = get-set-eq i v (tl s)

-- writing register j never disturbs a different register i.
get-set-neq : ∀ i j v s → ¬ (i ≡ j) → getR i (setR j v s) ≡ getR i s
get-set-neq zero    zero    v s neq = ⊥-elim (neq refl)
get-set-neq zero    (suc j) v s neq = refl
get-set-neq (suc i) zero    v s neq = refl
get-set-neq (suc i) (suc j) v s neq = get-set-neq i j v (tl s) (λ e → neq (cong suc e))

------------------------------------------------------------------------
-- Concrete check: a 3-register store; writing register 1 leaves 0 and 2 intact.
------------------------------------------------------------------------

a b c x : V
a = ⟨⟩
b = ⟨⟩ ∙ ⟨⟩
c = (⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩
x = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

store : V
store = a ∙ (b ∙ (c ∙ ⟨⟩))        -- registers [a, b, c]

_ : getR 1 store ≡ b
_ = refl

_ : getR 1 (setR 1 x store) ≡ x          -- read back the write
_ = refl

_ : getR 0 (setR 1 x store) ≡ a          -- register 0 untouched
_ = refl

_ : getR 2 (setR 1 x store) ≡ c          -- register 2 untouched
_ = refl
