{-# OPTIONS --safe #-}
------------------------------------------------------------------------
-- G1 (THE KEYSTONE): general self-interpreter correctness -- in progress.
--
-- Goal (`docs/PROOF-OBLIGATIONS.md` §G1): `SelfInterp.sint` computes the meaning
-- of EVERY program for ALL inputs.  Target semantics for the call-free fragment:
-- the total function `ev0`.  Statement:
--
--     ∀ e → <fragment> e → ∀ p env → Σ F, run sint F (⌜p⌝.(⌜e⌝.env)) ≡ just (ev0 e env)
--
-- DECOMPOSITION (the key to making this tractable).  Each recursive case splits
-- into THREE small pieces:
--   (1) a DISPATCH LEMMA (by `refl`): `run (D+F) sint (⌜C e⌝-input)` reduces,
--       through `sint`'s nested-`kifnil` dispatch, to a combinator of the
--       sub-request `run (J+F) sint (⌜e⌝-input)` (the constants `D,J` found by a
--       small fuel grid search -- the inner phase keeps `J+F` fuel, not `F`);
--   (2) `Mono.add-fuel` to lift the inductive hypothesis from fuel `Fₑ` to `J+Fₑ`;
--   (3) `trans`/`cong` to combine.
--
-- DONE here: the head/tail fragment `HT` (var/knil/khd/ktl) -- a complete,
-- machine-checked instance of G1.  REMAINING (`docs/PROOF-OBLIGATIONS.md` §G1):
-- the `kcons` case (dispatch lemma with TWO sub-requests) and the `kifnil` case
-- (dispatch reduces to a conditional on the sub-run), then the `call` case (an
-- induction on the object eval fuel) for the full language.
--
-- Referenced by docs/AGDA_CORRESPONDENCE.md and docs/PROOF-OBLIGATIONS.md (§G1).
------------------------------------------------------------------------

module SelfInterpCorrect where

open import Data.Nat using (ℕ; _+_; _∸_; _≤_)
open import Data.Nat.Properties using (+-comm; ≤-trans; m≤m+n; m≤n+m; m∸n+n≡m)
open import Data.Product using (Σ; _,_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; cong₂; trans; subst)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil; Exp; var; knil; kcons; khd; ktl; kifnil; call;
         just; _>>=_; module Interp; module Mono)
open import ProgEncoding using (⌜_⌝)
open import SelfInterp using (sint)

------------------------------------------------------------------------
-- The target: the meaning of a call-free program (a total function).
------------------------------------------------------------------------

ev0 : Exp → V → V
ev0 var          env = env
ev0 knil         env = ⟨⟩
ev0 (kcons a b)  env = ev0 a env ∙ ev0 b env
ev0 (khd e)      env = hd (ev0 e env)
ev0 (ktl e)      env = tl (ev0 e env)
ev0 (kifnil c t e) env = ifnil (ev0 c env) (ev0 t env) (ev0 e env)
ev0 (call e)     env = ⟨⟩          -- dummy; `call` is excluded by the fragment

-- the call-free fragment minus conditionals (var / knil / khd / ktl / kcons),
-- proved here.  Adding `kifnil` completes the call-free fragment (next).
data HT : Exp → Set where
  ht-var   : HT var
  ht-knil  : HT knil
  ht-khd   : ∀ {e} → HT e → HT (khd e)
  ht-ktl   : ∀ {e} → HT e → HT (ktl e)
  ht-kcons  : ∀ {a b} → HT a → HT b → HT (kcons a b)
  ht-kifnil : ∀ {c t e} → HT c → HT t → HT e → HT (kifnil c t e)

open Interp sint
open Mono sint using (add-fuel)

------------------------------------------------------------------------
-- Dispatch lemmas (each holds by `refl`; constants found by fuel grid search).
------------------------------------------------------------------------

khd-dispatch : ∀ p env e F →
  run (12 + F) (p ∙ (⌜ khd e ⌝ ∙ env)) ≡ (run (6 + F) (p ∙ (⌜ e ⌝ ∙ env))) >>= λ v → just (hd v)
khd-dispatch p env e F = refl

ktl-dispatch : ∀ p env e F →
  run (13 + F) (p ∙ (⌜ ktl e ⌝ ∙ env)) ≡ (run (6 + F) (p ∙ (⌜ e ⌝ ∙ env))) >>= λ v → just (tl v)
ktl-dispatch p env e F = refl

kcons-dispatch : ∀ p env a b F →
  run (12 + F) (p ∙ (⌜ kcons a b ⌝ ∙ env)) ≡
    (run (7 + F) (p ∙ (⌜ a ⌝ ∙ env))) >>= λ va →
    (run (7 + F) (p ∙ (⌜ b ⌝ ∙ env))) >>= λ vb → just (va ∙ vb)
kcons-dispatch p env a b F = refl

kifnil-dispatch : ∀ p env c t e F →
  run (16 + F) (p ∙ (⌜ kifnil c t e ⌝ ∙ env)) ≡
    (run (8 + F) (p ∙ (⌜ c ⌝ ∙ env))) >>= λ vc →
      ifnil vc (run (8 + F) (p ∙ (⌜ t ⌝ ∙ env))) (run (8 + F) (p ∙ (⌜ e ⌝ ∙ env)))
kifnil-dispatch p env c t e F = refl

-- fuel-monotonicity up to ≤ (so three sub-results can share one inner fuel).
mono-≤ : ∀ {n m x v} → n ≤ m → run n x ≡ just v → run m x ≡ just v
mono-≤ {n} {m} {x} {v} n≤m h =
  subst (λ z → run z x ≡ just v) (m∸n+n≡m n≤m) (add-fuel (m ∸ n) n sint x h)

-- the residual conditional commutes with `just` (case on the guard value).
ifnil-just : ∀ vc (x y : V) → ifnil vc (just x) (just y) ≡ just (ifnil vc x y)
ifnil-just ⟨⟩      x y = refl
ifnil-just (_ ∙ _) x y = refl

-- the kifnil combine: cond + both branches at the common fuel ⇒ the result.
kifnil-case : ∀ p env c t e F →
  run (8 + F) (p ∙ (⌜ c ⌝ ∙ env)) ≡ just (ev0 c env) →
  run (8 + F) (p ∙ (⌜ t ⌝ ∙ env)) ≡ just (ev0 t env) →
  run (8 + F) (p ∙ (⌜ e ⌝ ∙ env)) ≡ just (ev0 e env) →
  run (16 + F) (p ∙ (⌜ kifnil c t e ⌝ ∙ env)) ≡ just (ev0 (kifnil c t e) env)
kifnil-case p env c t e F hc ht he
  rewrite kifnil-dispatch p env c t e F | hc | ht | he =
    ifnil-just (ev0 c env) (ev0 t env) (ev0 e env)

------------------------------------------------------------------------
-- G1 for the head/tail fragment: sint correctly interprets every such program,
-- for all programs `p` and environments `env`.
------------------------------------------------------------------------

sim : ∀ e → HT e → ∀ p env → Σ ℕ (λ F → run F (p ∙ (⌜ e ⌝ ∙ env)) ≡ just (ev0 e env))
sim var  ht-var  p env = 8 , refl
sim knil ht-knil p env = 8 , refl
sim (khd e) (ht-khd he) p env with sim e he p env
... | Fe , ih = 12 + Fe ,
      trans (khd-dispatch p env e Fe)
            (cong (λ z → z >>= λ v → just (hd v))
                  (add-fuel 6 Fe sint (p ∙ (⌜ e ⌝ ∙ env)) ih))
sim (ktl e) (ht-ktl he) p env with sim e he p env
... | Fe , ih = 13 + Fe ,
      trans (ktl-dispatch p env e Fe)
            (cong (λ z → z >>= λ v → just (tl v))
                  (add-fuel 6 Fe sint (p ∙ (⌜ e ⌝ ∙ env)) ih))
sim (kcons a b) (ht-kcons ha hb) p env with sim a ha p env | sim b hb p env
... | Fa , iha | Fb , ihb = 12 + (Fa + Fb) ,
      trans (kcons-dispatch p env a b (Fa + Fb))
            (cong₂ (λ x y → x >>= λ va → y >>= λ vb → just (va ∙ vb)) iha' ihb')
  where
    -- bump each sub-result to the common inner fuel `7 + (Fa + Fb)`.
    ihb' : run (7 + (Fa + Fb)) (p ∙ (⌜ b ⌝ ∙ env)) ≡ just (ev0 b env)
    ihb' = add-fuel (7 + Fa) Fb sint (p ∙ (⌜ b ⌝ ∙ env)) ihb
    iha' : run (7 + (Fa + Fb)) (p ∙ (⌜ a ⌝ ∙ env)) ≡ just (ev0 a env)
    iha' = subst (λ n → run n (p ∙ (⌜ a ⌝ ∙ env)) ≡ just (ev0 a env))
                 (cong (7 +_) (+-comm Fb Fa))
                 (add-fuel (7 + Fb) Fa sint (p ∙ (⌜ a ⌝ ∙ env)) iha)
sim (kifnil c t e) (ht-kifnil hc ht he) p env
  with sim c hc p env | sim t ht p env | sim e he p env
... | Fc , ihc | Ft , iht | Fe , ihe = 16 + (Fc + Ft + Fe) ,
      kifnil-case p env c t e (Fc + Ft + Fe)
        (mono-≤ c≤ ihc) (mono-≤ t≤ iht) (mono-≤ e≤ ihe)
  where
    F = Fc + Ft + Fe
    c≤ : Fc ≤ 8 + F
    c≤ = ≤-trans (≤-trans (m≤m+n Fc Ft) (m≤m+n (Fc + Ft) Fe)) (m≤n+m F 8)
    t≤ : Ft ≤ 8 + F
    t≤ = ≤-trans (≤-trans (m≤n+m Ft Fc) (m≤m+n (Fc + Ft) Fe)) (m≤n+m F 8)
    e≤ : Fe ≤ 8 + F
    e≤ = ≤-trans (m≤n+m Fe (Fc + Ft)) (m≤n+m F 8)
