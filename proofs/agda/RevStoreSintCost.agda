{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin Track-2 (brick S4 proper; traversal-charge plan
-- task 4): the SELF-INTERPRETER'S OWN step count, walk charged --
-- `usintVC` charges each dispatch of `RevStoreInterp.usintV` its actual
-- work: 1 for the tag dispatch, the unary-numeral spine walk for reading
-- a variable operand (`unℕ`, the index-decode convention of RCmdVEnc),
-- and the store walk of the op it fires (`mapAtC`/`lookupC`).  Under the
-- side condition pinned by the convention -- every variable index is
-- below the store length, `varsBelow c L` with `L = length s` -- each
-- charge is ≤ (1+L) + (1+L), so the interpreter-level overhead is affine
-- in L with base 2:
--
--     usintVC n ⌜ c ⌝V s ≤ (2 + 2L) · ocostV c s     (sizeV c ≤ n)
--
-- (`usintVC-affine`, alias `int-efficient-rev-store`).  This discharges
-- the assumption-based envelope of `RevStoreOverhead.AffineInL` at the
-- interpreter level: the object-level walk (task 1, a-revW(L) = 1+L) and
-- the interpreter's numeral walk are BOTH charged, and the numeral walk
-- is legitimate as a traversal charge exactly because k < L makes it a
-- prefix of the Vl walk (main.tex §cost-model).  --safe, axiom-free.
------------------------------------------------------------------------

module RevStoreSintCost where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s)
open import Data.Nat.Properties
  using (≤-refl; ≤-trans; m≤m+n; m≤n+m; +-mono-≤; n≤1+n)
open import Data.List using (length)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; sym; trans; cong; subst)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)
open import RevStore
  using (Store; lookup; lookupC; lookupC-bound; mapAtC; mapAtC-bound)
open import RCmdV using (RCmdV; swpV; sqV; ifnV; fwdV; ocostV)
open import RCmdVWalkCost using (fwdV-length)
open import RCmdVEnc using (⌜_⌝V; ⌜_⌝ℕ; unℕ; unℕ-⌜⌝ℕ)
open import RevStoreInterp using (usintV; sizeV; usintV-correct)
open import RevLinearOverhead using (lin-leaf; lin-un; lin-bin)
import RCmdV

------------------------------------------------------------------------
-- the interpreter-level charge: dispatch (1) + operand numeral walk
-- (unℕ spine) + store walk (mapAtC/lookupC), clause-for-clause with
-- `usintV`.  Out of fuel = 0 (nothing dispatched); unrecognised code = 1
-- (one rejecting dispatch).

usintVC : ℕ → V → Store → ℕ
usintVC zero    code                                 s = 0
usintVC (suc n) (⟨⟩ ∙ k)                             s =
  suc (unℕ k + mapAtC (unℕ k) s)
usintVC (suc n) ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d))               s =
  1 + usintVC n c s + usintVC n d (usintV n c s)
usintVC (suc n) (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (k ∙ (c ∙ d))) s =
  suc (unℕ k + lookupC (unℕ k) s)
    + ifnil (lookup (unℕ k) s) (usintVC n c s) (usintVC n d s)
usintVC (suc n) _                                    s = 1

------------------------------------------------------------------------
-- the side condition of the index-decode convention: every variable
-- index of the program is below L (so the numeral walk is a prefix of
-- the length-L Vl walk).

varsBelow : RCmdV → ℕ → Set
varsBelow (swpV k)     L = suc k ≤ L
varsBelow (sqV c d)    L = varsBelow c L × varsBelow d L
varsBelow (ifnV k c d) L = suc k ≤ L × (varsBelow c L × varsBelow d L)

------------------------------------------------------------------------
-- the interpreter-level constant: base 2 (dispatch + one) + two walks.

a-revS : ℕ → ℕ
a-revS L = suc L + suc L      -- = 2 + 2L

-- a per-op head charge: numeral walk k plus store walk m, one dispatch.
walk-head : ∀ {k L} (m : ℕ) → suc k ≤ L → m ≤ L → suc (k + m) ≤ a-revS L
walk-head {k} {L} m hk hm =
  +-mono-≤ (≤-trans hk (n≤1+n L)) (≤-trans hm (n≤1+n L))

------------------------------------------------------------------------
-- THE THEOREM (S4 at interpreter level, walk charged): given fuel and
-- the k < L side condition, the self-interpreter's own step count is at
-- most (2 + 2L) times the fair unit cost of the interpreted program.

usintVC-affine : (c : RCmdV) (s : Store) (n : ℕ)
  → sizeV c ≤ n → varsBelow c (length s)
  → usintVC n ⌜ c ⌝V s ≤ a-revS (length s) * ocostV c s
usintVC-affine (swpV k) s (suc n) _ hk
  rewrite unℕ-⌜⌝ℕ k =
  lin-leaf (walk-head (mapAtC k s) hk (mapAtC-bound k s))
usintVC-affine (sqV c d) s (suc n) (s≤s le) (vc , vd) =
  lin-bin {κ = 1}
          {a = usintVC n ⌜ c ⌝V s}
          {b = usintVC n ⌜ d ⌝V (usintV n ⌜ c ⌝V s)}
          {x = ocostV c s} {y = ocostV d (fwdV c s)}
          {K = a-revS (length s)}
          (s≤s z≤n)
          (usintVC-affine c s n (≤-trans (m≤m+n (sizeV c) (sizeV d)) le) vc)
          ih-d
  where
    -- the inner store is `usintV n ⌜c⌝V s`; rewrite it to `fwdV c s`
    -- (usintV-correct), prove there, and transport L back (fwdV-length).
    ih-d0 : usintVC n ⌜ d ⌝V (fwdV c s)
          ≤ a-revS (length (fwdV c s)) * ocostV d (fwdV c s)
    ih-d0 = usintVC-affine d (fwdV c s) n
              (≤-trans (m≤n+m (sizeV d) (sizeV c)) le)
              (subst (varsBelow d) (sym (fwdV-length c s)) vd)

    ih-d1 : usintVC n ⌜ d ⌝V (fwdV c s)
          ≤ a-revS (length s) * ocostV d (fwdV c s)
    ih-d1 = subst (λ l → usintVC n ⌜ d ⌝V (fwdV c s)
                       ≤ a-revS l * ocostV d (fwdV c s))
                  (fwdV-length c s) ih-d0

    ih-d : usintVC n ⌜ d ⌝V (usintV n ⌜ c ⌝V s)
         ≤ a-revS (length s) * ocostV d (fwdV c s)
    ih-d = subst (λ t → usintVC n ⌜ d ⌝V t
                      ≤ a-revS (length s) * ocostV d (fwdV c s))
                 (sym (usintV-correct c s n
                        (≤-trans (m≤m+n (sizeV c) (sizeV d)) le)))
                 ih-d1
usintVC-affine (ifnV k c d) s (suc n) (s≤s le) (hk , vc , vd)
  rewrite unℕ-⌜⌝ℕ k
  with lookup k s
... | ⟨⟩    = lin-un (walk-head (lookupC k s) hk (lookupC-bound k s))
                     (usintVC-affine c s n
                       (≤-trans (m≤m+n (sizeV c) (sizeV d)) le) vc)
... | _ ∙ _ = lin-un (walk-head (lookupC k s) hk (lookupC-bound k s))
                     (usintVC-affine d s n
                       (≤-trans (m≤n+m (sizeV d) (sizeV c)) le) vd)

-- the plan-line name: interpretation over the efficient reversible store
-- is affine in L at the interpreter's own step count.
int-efficient-rev-store : (c : RCmdV) (s : Store) (n : ℕ)
  → sizeV c ≤ n → varsBelow c (length s)
  → usintVC n ⌜ c ⌝V s ≤ a-revS (length s) * ocostV c s
int-efficient-rev-store = usintVC-affine

------------------------------------------------------------------------
-- Test-first: concrete charges, the bound on the standard two-variable
-- run, and the shape of the constant.

module Examples where
  open RCmdV.Examples using (s0; prog)   -- L = 2; prog = sqV (swpV 0) (ifnV 1 …)

  -- one swpV 0, interpreted: 1 dispatch + 0 numeral walk + the mapAt walk.
  ex-swp-cost : usintVC 1 ⌜ swpV 0 ⌝V s0 ≡ suc (mapAtC 0 s0)
  ex-swp-cost = refl

  -- out of fuel charges nothing; garbage code charges one dispatch.
  ex-nofuel : usintVC 0 ⌜ prog ⌝V s0 ≡ 0
  ex-nofuel = refl

  ex-garbage : usintVC 5 ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) s0 ≡ 1
  ex-garbage = refl

  -- the side condition holds for prog at L = 2 (indices 0 and 1)...
  ex-vars : varsBelow prog 2
  ex-vars = (s≤s z≤n , (s≤s (s≤s z≤n) , (s≤s z≤n , s≤s z≤n)))

  -- ...and the headline bound fires on the concrete instance.
  ex-bound : usintVC (sizeV prog) ⌜ prog ⌝V s0
           ≤ a-revS 2 * ocostV prog s0
  ex-bound = usintVC-affine prog s0 (sizeV prog) ≤-refl ex-vars

  -- the constant: base 2, slope 2 -- a-revS L = 2 + 2L.
  ex-base : a-revS 0 ≡ 2
  ex-base = refl

  ex-2 : a-revS 2 ≡ 6
  ex-2 = refl

  -- L = 75 (the ri.rwhile Vl size): 2 + 150.
  ex-75 : a-revS 75 ≡ 152
  ex-75 = refl
