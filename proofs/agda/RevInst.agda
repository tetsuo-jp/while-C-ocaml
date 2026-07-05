{-# OPTIONS --safe #-}

------------------------------------------------------------------------
-- Reversible Levin R3a+R3b: a concrete, ENCODABLE reversible language `RCmd`.
--
-- `RevCore.Language` studies reversibility abstractly, but its `atom : Upd → Cmd`
-- carries a FUNCTION (`Σ (S→S) involution`), so `Cmd` is not data and admits no
-- self-interpreter.  R3 needs a first-order object language with FINITE
-- constructors and an encoding `⌜_⌝ : RCmd → V` (the `Exp`/`ProgEncoding` twin) --
-- the substrate for the reversible self-interpreter (R3c) and `a-rev` (R3d).
--
-- `RCmd` over ONE tree variable: `swp` (swap the root's two subtrees, self-inverse),
-- `sq`, `ifn` (branch on nil).  Every op PRESERVES nil-ness, so the `ifn` guard is
-- recovered on reversal WITHOUT any well-formedness hypothesis -- `inv-sound` is
-- UNCONDITIONAL here (cleaner than `RevCore`, whose abstract `cond` needed `WF`).
------------------------------------------------------------------------

module RevInst where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm)
open import Data.Bool using (Bool; true; false)
open import Data.Product using (_×_; _,_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; sym; trans; cong; cong₂)
open import UniversalInterp using (V; ⟨⟩; _∙_; ifnil)

------------------------------------------------------------------------
-- primitives
------------------------------------------------------------------------

swapTop : V → V
swapTop ⟨⟩      = ⟨⟩
swapTop (a ∙ b) = b ∙ a

swapTop-invol : (v : V) → swapTop (swapTop v) ≡ v
swapTop-invol ⟨⟩      = refl
swapTop-invol (a ∙ b) = refl

isNil : V → Bool
isNil ⟨⟩      = true
isNil (_ ∙ _) = false

ifnil-then : {A : Set} (w : V) (a b : A) → isNil w ≡ true  → ifnil w a b ≡ a
ifnil-then ⟨⟩      a b _ = refl
ifnil-then (_ ∙ _) a b ()
ifnil-else : {A : Set} (w : V) (a b : A) → isNil w ≡ false → ifnil w a b ≡ b
ifnil-else ⟨⟩      a b ()
ifnil-else (_ ∙ _) a b _ = refl

------------------------------------------------------------------------
-- the concrete reversible language, its semantics and inversion
------------------------------------------------------------------------

data RCmd : Set where
  swp : RCmd
  sq  : RCmd → RCmd → RCmd
  ifn : RCmd → RCmd → RCmd

fwd : RCmd → V → V
fwd swp      s = swapTop s
fwd (sq c d) s = fwd d (fwd c s)
fwd (ifn c d) s = ifnil s (fwd c s) (fwd d s)

inv : RCmd → RCmd
inv swp      = swp
inv (sq c d) = sq (inv d) (inv c)
inv (ifn c d) = ifn (inv c) (inv d)

bwd : RCmd → V → V
bwd c = fwd (inv c)

inv-inv : (c : RCmd) → inv (inv c) ≡ c
inv-inv swp      = refl
inv-inv (sq c d) = cong₂ sq (inv-inv c) (inv-inv d)
inv-inv (ifn c d) = cong₂ ifn (inv-inv c) (inv-inv d)

-- every command preserves nil-ness (so the ifn guard survives inversion)
preservesNil : (c : RCmd) (s : V) → isNil (fwd c s) ≡ isNil s
preservesNil swp ⟨⟩      = refl
preservesNil swp (a ∙ b) = refl
preservesNil (sq c d) s  = trans (preservesNil d (fwd c s)) (preservesNil c s)
preservesNil (ifn c d) ⟨⟩      = preservesNil c ⟨⟩
preservesNil (ifn c d) (a ∙ b) = preservesNil d (a ∙ b)

-- KEYSTONE (R3a): running the inverse forward undoes the forward run, UNCONDITIONALLY.
inv-sound : (c : RCmd) (s : V) → bwd c (fwd c s) ≡ s
inv-sound swp      s = swapTop-invol s
inv-sound (sq c d) s rewrite inv-sound d (fwd c s) = inv-sound c s
inv-sound (ifn c d) ⟨⟩ =
  trans (ifnil-then (fwd c ⟨⟩) (bwd c (fwd c ⟨⟩)) (bwd d (fwd c ⟨⟩)) (preservesNil c ⟨⟩))
        (inv-sound c ⟨⟩)
inv-sound (ifn c d) (a ∙ b) =
  trans (ifnil-else (fwd d (a ∙ b)) (bwd c (fwd d (a ∙ b))) (bwd d (fwd d (a ∙ b))) (preservesNil d (a ∙ b)))
        (inv-sound d (a ∙ b))

------------------------------------------------------------------------
-- step count and reversible time-symmetry (the RevCore twins)
------------------------------------------------------------------------

cost : RCmd → V → ℕ
cost swp      s = 1
cost (sq c d) s = cost c s + cost d (fwd c s)
cost (ifn c d) s = suc (ifnil s (cost c s) (cost d s))

cost-sym : (c : RCmd) (s : V) → cost (inv c) (fwd c s) ≡ cost c s
cost-sym swp      s = refl
cost-sym (sq c d) s
  rewrite inv-sound d (fwd c s) | cost-sym d (fwd c s) | cost-sym c s
  = +-comm (cost d (fwd c s)) (cost c s)
cost-sym (ifn c d) ⟨⟩ =
  cong suc (trans (ifnil-then (fwd c ⟨⟩) (cost (inv c) (fwd c ⟨⟩)) (cost (inv d) (fwd c ⟨⟩)) (preservesNil c ⟨⟩))
                  (cost-sym c ⟨⟩))
cost-sym (ifn c d) (a ∙ b) =
  cong suc (trans (ifnil-else (fwd d (a ∙ b)) (cost (inv c) (fwd d (a ∙ b))) (cost (inv d) (fwd d (a ∙ b))) (preservesNil d (a ∙ b)))
                  (cost-sym d (a ∙ b)))

------------------------------------------------------------------------
-- R3b: the ENCODING `⌜_⌝ : RCmd → V` -- the point of the concrete language.
-- Tags are unary numerals; `decode` is a left inverse.  (This is what abstract
-- `RevCore.Cmd` could NOT have, and what the self-interpreter will consume.)
------------------------------------------------------------------------

open import Data.Maybe using (Maybe; just; nothing)

⌜_⌝ : RCmd → V
⌜ swp ⌝     = ⟨⟩                              -- tag 0
⌜ sq c d ⌝  = (⟨⟩ ∙ ⟨⟩) ∙ (⌜ c ⌝ ∙ ⌜ d ⌝)    -- tag 1 . (⌜c⌝ . ⌜d⌝)
⌜ ifn c d ⌝ = ((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (⌜ c ⌝ ∙ ⌜ d ⌝)  -- tag 2 . (⌜c⌝ . ⌜d⌝)

decode : V → Maybe RCmd
decode ⟨⟩ = just swp
decode ((⟨⟩ ∙ ⟨⟩) ∙ (c ∙ d)) with decode c | decode d
... | just c' | just d' = just (sq c' d')
... | _       | _       = nothing
decode (((⟨⟩ ∙ ⟨⟩) ∙ ⟨⟩) ∙ (c ∙ d)) with decode c | decode d
... | just c' | just d' = just (ifn c' d')
... | _       | _       = nothing
decode _ = nothing

decode-⌜⌝ : (c : RCmd) → decode ⌜ c ⌝ ≡ just c
decode-⌜⌝ swp = refl
decode-⌜⌝ (sq c d)  rewrite decode-⌜⌝ c | decode-⌜⌝ d = refl
decode-⌜⌝ (ifn c d) rewrite decode-⌜⌝ c | decode-⌜⌝ d = refl
