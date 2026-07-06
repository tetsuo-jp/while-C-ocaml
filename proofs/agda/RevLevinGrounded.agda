{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3e / Track 1 (brick E1): REVERSIBLE LEVIN OPTIMALITY with the
-- interpreter-overhead hypothesis DISCHARGED -- the `LevinGrounded` twin.
--
-- `RevLevinAbstract.RevSearch` (R0a) fires Layer A verbatim at an ABSTRACT
-- reversible overhead constant `a-rev`, giving `gsum k < 4·a-rev·2^j·(T_q+T_r)`.
-- `RevSintCost.int-efficient-rev` (R3d) proved the self-interpreter `usint` runs
-- one object step in ≤ `a-rev = 4` of its own steps.  This module FEEDS that
-- proven constant into Layer A: every reversible search record carries
-- `overhead = a-rev`, and `overhead-grounded` witnesses that this exact constant
-- is the machine-checked interpreter bound (not an assumption).  So
-- `rev-levin-grounded` is reversible Levin optimality with `a-rev` a THEOREM.
--
-- This makes the reversible-Levin time-optimality statement CLAIMABLE at the
-- DRIVER level (interpreter = the semantic `usint`; the program-level `rsint` and
-- the 364× store-walk model are the fidelity upgrades of Tracks 2/3, exactly the
-- relation classical B3d bears to Thm 20.1.2).  --safe, axiom-free.
------------------------------------------------------------------------

module RevLevinGrounded where

open import Data.Nat using (ℕ; suc; _+_; _*_; _≤_; _<_; s≤s; z≤n)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

open import UniversalInterp using (V)
import RevInst as R
open R using (⌜_⌝)
open import RevSelfInterp using (size)
open import RevSintCost using (usintC; ocost; a-rev; int-efficient-rev)
open import LevinBudget using (gsum)
open import LevinOptimal using (LevinSearch; aq; c; S)
import RevLevinAbstract as RLA

------------------------------------------------------------------------
-- The reversible linear-overhead property, as a proposition on the constant `ov`:
-- self-interpreting any `c` (given enough fuel) costs `usintC` at most `ov · ocost`.
-- `RevSintCost.int-efficient-rev` is exactly an inhabitant at `ov = a-rev = 4`.
------------------------------------------------------------------------

LinearOverheadRev : ℕ → Set
LinearOverheadRev ov =
  (cc : R.RCmd) (s : V) (n : ℕ) → size cc ≤ n → usintC n ⌜ cc ⌝ s ≤ ov * ocost cc s

overhead-grounded : LinearOverheadRev a-rev
overhead-grounded = int-efficient-rev

------------------------------------------------------------------------
-- Feed the proven constant into Layer A's abstract reversible search record.
------------------------------------------------------------------------

rev-search : ℕ → ℕ → ℕ → LevinSearch
rev-search j Tq Tr = RLA.RevSearch.rev-search a-rev j Tq Tr

-- the overhead field is literally the machine-checked interpreter bound a-rev = 4.
overhead-is-a-rev : ∀ j Tq Tr → LevinSearch.overhead (rev-search j Tq Tr) ≡ a-rev
overhead-is-a-rev _ _ _ = refl

_ : LevinSearch.overhead (rev-search 0 0 0) ≡ 4
_ = refl

------------------------------------------------------------------------
-- REVERSIBLE LEVIN OPTIMALITY with `a-rev` discharged.  For every solver of index
-- `j` and times `T_q, T_r` (positive target), the reversible dovetail finishes in a
-- phase of cumulative cost < a_q·(T_q+T_r), a_q = 4·a-rev·2^j -- the reversible
-- interpreter overhead is now a THEOREM.
------------------------------------------------------------------------

rev-levin-grounded : ∀ j Tq Tr → 1 ≤ c (rev-search j Tq Tr) * S (rev-search j Tq Tr) →
  Σ ℕ (λ k → gsum k < aq (rev-search j Tq Tr) * S (rev-search j Tq Tr))
rev-levin-grounded j Tq Tr = RLA.RevSearch.rev-levin-optimal a-rev j Tq Tr

-- Concrete: index 0, T_q = 3, T_r = 1  ⇒  overhead = 4, S = 4, a_q = 4·4·1 = 16.
_ : Σ ℕ (λ k → gsum k < aq (rev-search 0 3 1) * S (rev-search 0 3 1))
_ = rev-levin-grounded 0 3 1 (s≤s z≤n)
