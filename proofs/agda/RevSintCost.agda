{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3d (fuel-level): the reversible interpreter overhead is
-- LINEAR -- the reversible twin of the classical `IntEfficient` (`a = 73`).
--
-- `RevSelfInterp.usint` self-interprets `⌜c⌝` correctly.  Here we equip it with a
-- STEP COUNT `usintC` (per-op dispatch constants reflecting the tag-inspection
-- depth of `RevInst`'s encoding: swp = 1, sq = 3, ifn = 4) and a faithful
-- object-program step count `ocost` (one step per executed RCmd node; for `ifn`
-- only the TAKEN branch, via `ifnil` -- so the interpreter never runs the dead
-- branch).  The theorem
--
--     int-efficient-rev : size c ≤ n → usintC n ⌜c⌝ s ≤ a-rev * ocost c s
--
-- with a-rev = 4 says: the self-interpreter simulates each object step with a
-- BOUNDED number of its own steps -- linear overhead, a THEOREM (the `IntEfficient`
-- twin).  The CONSTANT is small here because this minimal language has O(1) tag
-- dispatch (no variable-list `Vl` walk); the measured `a-rev ≈ 364×` of
-- `rwhile-C-ocaml/ri.rwhile` comes from that `Vl` walk, whose cost model is future
-- work (R3d full).  Per the skeleton, the theorem is "linear overhead exists"; the
-- constant is model-dependent and secondary (as `a = 73` was).
------------------------------------------------------------------------

module RevSintCost where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-mono-≤; +-monoʳ-≤; m≤m+n; m≤n+m; +-comm)
open import Data.Nat.Solver using (module +-*-Solver)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; cong; subst; trans)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevSelfInterp using (usint; usint-correct; size)
import RevInst as R
open R using (⌜_⌝)

open +-*-Solver

------------------------------------------------------------------------
-- The reversible interpreter overhead constant (this model).

a-rev : ℕ
a-rev = 4

------------------------------------------------------------------------
-- faithful object step count: one step per EXECUTED RCmd node (ifn runs only the
-- taken branch).  A fair unit-cost model (charges the sequencing node too).

ocost : R.RCmd → V → ℕ
ocost R.swp      s = 1
ocost (R.sq c d)  s = 1 + ocost c s + ocost d (R.fwd c s)
ocost (R.ifn c d) s = 1 + ifnil s (ocost c s) (ocost d s)

------------------------------------------------------------------------
-- the self-interpreter's own step count, mirroring `usint`'s dispatch: a per-op
-- constant (tag-inspection depth) + the recursive sub-interpretations.

usintC : ℕ → V → V → ℕ
usintC zero    code                          s = 0
usintC (suc n) ⟨⟩                            s = 1
usintC (suc n) ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d))        s = 3 + usintC n c s + usintC n d (usint n c s)
usintC (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) s = 4 + ifnil s (usintC n c s) (usintC n d s)
usintC (suc n) _                             s = 0

------------------------------------------------------------------------
-- arithmetic reshapes (linear, discharged by the ring solver)

lem2 : ∀ x → 4 * (1 + x) ≡ 4 + 4 * x
lem2 = solve 1 (λ x → con 4 :* (con 1 :+ x) := con 4 :+ con 4 :* x) refl

lem4 : ∀ x y → 4 * (1 + x + y) ≡ (4 + 4 * x) + 4 * y
lem4 = solve 2 (λ x y → con 4 :* (con 1 :+ x :+ y) := (con 4 :+ con 4 :* x) :+ con 4 :* y) refl

3≤4 : 3 ≤ 4
3≤4 = s≤s (s≤s (s≤s z≤n))

------------------------------------------------------------------------
-- R3d KEYSTONE: linear reversible interpreter overhead (fuel-level).

int-efficient-rev : (c : R.RCmd) (s : V) (n : ℕ)
                  → size c ≤ n → usintC n ⌜ c ⌝ s ≤ a-rev * ocost c s
int-efficient-rev R.swp      s zero ()
int-efficient-rev (R.sq c d)  s zero ()
int-efficient-rev (R.ifn c d) s zero ()
int-efficient-rev R.swp s (suc n) _ = s≤s z≤n
int-efficient-rev (R.sq c d) s (suc n) (s≤s le)
  rewrite usint-correct c s n (≤-trans (m≤m+n (size c) (size d)) le)
        | lem4 (ocost c s) (ocost d (R.fwd c s))
  = +-mono-≤ (+-mono-≤ 3≤4 (int-efficient-rev c s n (≤-trans (m≤m+n (size c) (size d)) le)))
             (int-efficient-rev d (R.fwd c s) n (≤-trans (m≤n+m (size d) (size c)) le))
int-efficient-rev (R.ifn c d) ⟨⟩ (suc n) (s≤s le)
  rewrite lem2 (ocost c ⟨⟩)
  = +-monoʳ-≤ 4 (int-efficient-rev c ⟨⟩ n (≤-trans (m≤m+n (size c) (size d)) le))
int-efficient-rev (R.ifn c d) (a ∙ b) (suc n) (s≤s le)
  rewrite lem2 (ocost d (a ∙ b))
  = +-monoʳ-≤ 4 (int-efficient-rev d (a ∙ b) n (≤-trans (m≤n+m (size d) (size c)) le))

-- exact-fuel corollary (the headline overhead bound).
int-efficient-rev-exact : (c : R.RCmd) (s : V) → usintC (size c) ⌜ c ⌝ s ≤ a-rev * ocost c s
int-efficient-rev-exact c s = int-efficient-rev c s (size c) ≤-refl

------------------------------------------------------------------------
-- Test-first: concrete step counts and the bound on a real program.

module Examples where
  prog : R.RCmd
  prog = R.sq R.swp (R.ifn R.swp R.swp)

  s0 : V
  s0 = ⟨⟩ ∙ (⟨⟩ ∙ ⟨⟩)

  -- concrete interpreter- and object-step counts …
  ex-usintC : usintC (size prog) ⌜ prog ⌝ s0 ≡ 9
  ex-usintC = refl

  ex-ocost : ocost prog s0 ≡ 4
  ex-ocost = refl

  -- … and the overhead bound holds (9 ≤ 4 * 4 = 16).
  ex-bound : usintC (size prog) ⌜ prog ⌝ s0 ≤ a-rev * ocost prog s0
  ex-bound = int-efficient-rev-exact prog s0

------------------------------------------------------------------------
-- REVERSIBLE TIME-SYMMETRY of the self-interpreter (the RevInst.cost-sym twin,
-- lifted to `usintC`): self-interpreting the INVERSE program on the forward
-- result costs the self-interpreter EXACTLY as much as self-interpreting the
-- forward program -- so `a-rev` is a two-sided constant (backward self-
-- interpretation is as cheap as forward, the operational content of "uncomputing
-- costs the same as computing", now at the SELF-INTERPRETER level).
------------------------------------------------------------------------

-- inversion preserves code size (so exact fuel transfers to the inverse).
size-inv : (c : R.RCmd) → size (R.inv c) ≡ size c
size-inv R.swp      = refl
size-inv (R.sq c d)  rewrite size-inv d | size-inv c = cong suc (+-comm (size d) (size c))
size-inv (R.ifn c d) rewrite size-inv c | size-inv d = refl

symArith : ∀ a b → 3 + a + b ≡ 3 + b + a
symArith = solve 2 (λ a b → con 3 :+ a :+ b := con 3 :+ b :+ a) refl

usintC-sym : (c : R.RCmd) (s : V) (n : ℕ)
           → size c ≤ n → usintC n ⌜ R.inv c ⌝ (R.fwd c s) ≡ usintC n ⌜ c ⌝ s
usintC-sym R.swp      s zero ()
usintC-sym (R.sq c d)  s zero ()
usintC-sym (R.ifn c d) s zero ()
usintC-sym R.swp s (suc n) _ = refl
usintC-sym (R.sq c d) s (suc n) (s≤s le)
  rewrite trans (usint-correct (R.inv d) (R.fwd d (R.fwd c s)) n
                   (subst (λ x → x ≤ n) (sym (size-inv d)) (≤-trans (m≤n+m (size d) (size c)) le)))
                (R.inv-sound d (R.fwd c s))
        | usintC-sym d (R.fwd c s) n (≤-trans (m≤n+m (size d) (size c)) le)
        | usintC-sym c s n (≤-trans (m≤m+n (size c) (size d)) le)
        | usint-correct c s n (≤-trans (m≤m+n (size c) (size d)) le)
  = symArith (usintC n ⌜ d ⌝ (R.fwd c s)) (usintC n ⌜ c ⌝ s)
usintC-sym (R.ifn c d) ⟨⟩ (suc n) (s≤s le)
  rewrite R.ifnil-then (R.fwd c ⟨⟩) (usintC n ⌜ R.inv c ⌝ (R.fwd c ⟨⟩))
                       (usintC n ⌜ R.inv d ⌝ (R.fwd c ⟨⟩)) (R.preservesNil c ⟨⟩)
        | usintC-sym c ⟨⟩ n (≤-trans (m≤m+n (size c) (size d)) le)
  = refl
usintC-sym (R.ifn c d) (a ∙ b) (suc n) (s≤s le)
  rewrite R.ifnil-else (R.fwd d (a ∙ b)) (usintC n ⌜ R.inv c ⌝ (R.fwd d (a ∙ b)))
                       (usintC n ⌜ R.inv d ⌝ (R.fwd d (a ∙ b))) (R.preservesNil d (a ∙ b))
        | usintC-sym d (a ∙ b) n (≤-trans (m≤n+m (size d) (size c)) le)
  = refl

-- exact-fuel corollary: the backward self-interpretation cost equals the forward.
usintC-sym-exact : (c : R.RCmd) (s : V)
  → usintC (size c) ⌜ R.inv c ⌝ (R.fwd c s) ≡ usintC (size c) ⌜ c ⌝ s
usintC-sym-exact c s = usintC-sym c s (size c) ≤-refl
