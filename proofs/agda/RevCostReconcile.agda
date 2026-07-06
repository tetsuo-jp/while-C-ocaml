{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-3 (brick P5): COST-MODEL RECONCILIATION.
--
-- Four overhead constants for the reversible self-interpreter have been
-- machine-checked or measured, and they look different (4, base+2L, ~2,
-- ~364).  They are NOT in tension: each bounds a DIFFERENT interpreter step
-- count against the SAME canonical object cost `ocost` (one step per executed
-- RCmd node), and all four are instances of one affine law
--
--     icost ≤ (base + 2·m·L) · ocost
--
-- read at different store sizes L and different unit granularities.  This leaf
-- module states that law, exhibits the four proven bounds as members of one
-- family (`OverheadBound`), and gives a numeric bridge to the measurement.
--
--   unit of "1 interpreter step"        constant        source
--   --------------------------------    -----------     ------------------------
--   tag-match on the code tree (L=0)    a-rev = 4       RevSintCost.int-efficient-rev
--   dispatch + one length-L cell        base + 2mL      RevStoreOverhead.AffineInL
--   one agenda pop+dispatch             a-revL ≤ 2      RevAgenda.asteps-agenda
--   one ri.rwhile command (L≈75)        ≈364 (228–466)  measure_ri_overhead.sh
--
-- CANONICAL choice (resolves the skeleton's "which count is canonical" caveat):
-- the node-summed law `base + 2·m·L · ocost`.  a-rev = 4 is its L = 0 floor (the
-- theorem-level constant `RevLevinGrounded` cites); 364 is the L ≈ 75 instance.
--
-- HONESTY: the specific `base = 64, m = 2` that hit the measured MEAN 364 exactly
-- (`mean-exact`) are an ILLUSTRATIVE fit -- they show the measurement is
-- CONSISTENT with the affine law at plausible (base, m, L), not a constant
-- derived from `ri.rwhile`'s source.  The THEOREM content is the affine law
-- itself plus: m = 1 provably undershoots the band (so ≥2 accesses/op are
-- forced) and m ∈ {2,3} brackets the measured 228–466.  Constants are
-- model-dependent and secondary (as classical `a = 73` was).
-- --safe, axiom-free.
------------------------------------------------------------------------

module RevCostReconcile where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; _<_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; +-monoʳ-≤; m≤m+n; *-zeroʳ; +-identityʳ)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; subst)
open import UniversalInterp using (V)
open import RevSelfInterp using (size)
import RevSintCost as SC
open SC using (ocost; usintC; int-efficient-rev)
open import RevStoreOverhead using (module AffineInL; double-≡)
open import RevAgenda using (Agenda; agenda-correct; asteps; asteps-agenda)
import RevInst as R
open R using (⌜_⌝)

------------------------------------------------------------------------
-- The shared law: `icost` has linear overhead `a` against the canonical
-- object cost `ocost` (one step per executed RCmd node).

OverheadBound : (ℕ → V → V → ℕ) → ℕ → Set
OverheadBound icost a =
  (c : R.RCmd) (s : V) (n : ℕ) → size c ≤ n → icost n ⌜ c ⌝ s ≤ a * ocost c s

------------------------------------------------------------------------
-- The two structural instances -- exact type matches, zero proof work.

-- (1) meta-dispatch, L = 0: the interpreter's own tag inspection.
meta-bound : OverheadBound usintC SC.a-rev
meta-bound = int-efficient-rev

-- (2) store-walk, m = 1 lookup+update per op, store size L.
store-bound : (base L : ℕ) → OverheadBound (AffineInL.gcost base L) (AffineInL.a-rev base L)
store-bound base L = AffineInL.overhead-affine base L

------------------------------------------------------------------------
-- The per-op access multiplier `m` (the honest 364 shape): if the interpreter
-- makes `m` lookup+update pairs per object op, each ≤ L, the per-op charge is
-- base + 2·m·L.  This needs NO new theorem: it is Track 2's bound at store L' = m·L.

a-rev-model : (base m L : ℕ) → ℕ
a-rev-model base m L = base + 2 * (m * L)

-- the multiplier collapses definitionally into the affine bound at L' = m·L.
model≡affine : ∀ base m L → a-rev-model base m L ≡ AffineInL.a-rev base (m * L)
model≡affine base m L = refl

model-bound : ∀ base m L
            → OverheadBound (AffineInL.gcost base (m * L)) (a-rev-model base m L)
model-bound base m L = AffineInL.overhead-affine base (m * L)

-- the L = 0 floor: no store ⇒ the model collapses to the meta-dispatch constant.
floor : ∀ base m → a-rev-model base m 0 ≡ base
floor base m rewrite *-zeroʳ m = +-identityʳ base

floor-4 : a-rev-model 4 2 0 ≡ SC.a-rev
floor-4 = refl

------------------------------------------------------------------------
-- (3) agenda / program level: the loop-form self-interpreter's step count is
-- `ocost + 1` (`RevAgenda.asteps-agenda`), so it too is linear with constant 2
-- -- the same growth law at agenda granularity (stack ops free).

ocost-pos : (c : R.RCmd) (s : V) → 1 ≤ ocost c s
ocost-pos R.swp       s = s≤s z≤n
ocost-pos (R.sq c d)  s = s≤s z≤n
ocost-pos (R.ifn c d) s = s≤s z≤n

a-rev-agenda : ℕ
a-rev-agenda = 2

agenda-bound : (c : R.RCmd) (s : V)
             → asteps (agenda-correct c s) ≤ a-rev-agenda * ocost c s
agenda-bound c s rewrite asteps-agenda c s =
  subst (ocost c s + 1 ≤_) (sym (double-≡ (ocost c s)))
        (+-monoʳ-≤ (ocost c s) (ocost-pos c s))

------------------------------------------------------------------------
-- (4) the numeric bridge to the MEASURED band 228–466 (all `refl` / `m≤m+n`;
-- no decidability needed -- `m≤m+n a b : a ≤ a + b` and `a + b` normalises).

Band : ℕ → Set                              -- the measured band 228 ≤ x ≤ 466
Band x = (228 ≤ x) × (x ≤ 466)

-- m = 1 is REFUTED by the measurement: 4 + 2·75 = 154 < 228 -- so ≥2
-- accesses/op are FORCED (this is a real consequence, not a fit).
single-walk-below-band : AffineInL.a-rev 4 75 < 228
single-walk-below-band = m≤m+n 155 73

-- m ∈ {2,3} lands inside the measured band, bracketing the 228–466 spread.
bridge-m2 : Band (a-rev-model 4 2 75)        -- 304 ∈ [228,466]
bridge-m2 = m≤m+n 228 76 , m≤m+n 304 162

bridge-m3 : Band (a-rev-model 4 3 75)        -- 454 ∈ [228,466]
bridge-m3 = m≤m+n 228 226 , m≤m+n 454 12

-- ILLUSTRATIVE fit (see header HONESTY note): base = 64 (the classical
-- `call`-dispatch magnitude), m = 2, L = 75 reproduces the measured MEAN --
-- evidence of consistency with the affine law, NOT a derived constant.
mean-exact : a-rev-model 64 2 75 ≡ 364
mean-exact = refl

mean-in-band : Band (a-rev-model 64 2 75)
mean-in-band = m≤m+n 228 136 , m≤m+n 364 102

------------------------------------------------------------------------
-- The four constants, ordered: agenda floor ≤ meta floor ≤ store(L=75,m=1)
-- ≤ store(L=75,m=2).  One monotone chain, one growth law.

chain : (a-rev-agenda ≤ SC.a-rev)                     -- 2 ≤ 4
      × (SC.a-rev ≤ AffineInL.a-rev 4 75)             -- 4 ≤ 154
      × (AffineInL.a-rev 4 75 ≤ a-rev-model 4 2 75)   -- 154 ≤ 304
chain = s≤s (s≤s z≤n) , m≤m+n 4 150 , m≤m+n 154 150

------------------------------------------------------------------------
-- Test-first: the constants table, refl-checked.

module Examples where
  meta-floor : a-rev-model 4 2 0 ≡ 4
  meta-floor = refl

  store-m1 : AffineInL.a-rev 4 75 ≡ 154
  store-m1 = refl

  store-m2 : a-rev-model 4 2 75 ≡ 304
  store-m2 = refl

  store-m3 : a-rev-model 4 3 75 ≡ 454
  store-m3 = refl
