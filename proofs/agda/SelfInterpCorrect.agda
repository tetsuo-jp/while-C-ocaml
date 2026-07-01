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

open import Data.Nat using (ℕ; zero; suc; _+_; _∸_; _≤_)
open import Data.Nat.Properties using (+-comm; ≤-trans; m≤m+n; m≤n+m; m∸n+n≡m)
open import Data.Product using (Σ; _,_)
open import Data.Empty using (⊥; ⊥-elim)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; cong₂; trans; subst)
open import UniversalInterp
  using (V; ⟨⟩; _∙_; hd; tl; ifnil; Exp; var; knil; kcons; khd; ktl; kifnil; call;
         nothing; just; _>>=_; module Interp; module Mono)
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
-- The `call` case (object recursion).  `call e` re-interprets the program `cp`
-- with env = ⟦e⟧.  The dispatch builds the request `(cp . (cp . ⟦e⟧))` via TWO
-- `kcons` (a 3-level nested bind, found by fuel grid search: 18 / 8 / 11).  Once
-- the inner self-interp of `e` resolves to `arg`, the binds collapse and the
-- outer self-interp re-runs the whole program on `arg`.
------------------------------------------------------------------------

call-dispatch : ∀ cp env e F →
  run (18 + F) (cp ∙ (⌜ call e ⌝ ∙ env)) ≡
    ((run (8 + F) (cp ∙ (⌜ e ⌝ ∙ env)) >>= λ iv → just (cp ∙ iv))
        >>= λ vy → just (cp ∙ vy))
      >>= λ req → run (11 + F) req
call-dispatch cp env e F = refl

call-case : ∀ cp env e F {arg v} →
  run (8 + F) (cp ∙ (⌜ e ⌝ ∙ env)) ≡ just arg →
  run (11 + F) (cp ∙ (cp ∙ arg)) ≡ just v →
  run (18 + F) (cp ∙ (⌜ call e ⌝ ∙ env)) ≡ just v
call-case cp env e F he hp
  rewrite call-dispatch cp env e F | he = hp

------------------------------------------------------------------------
-- FULL G1 (Thm 4.2.1, ∀ form).  Per-constructor "case" combinators take the
-- sub-results (at the dispatch's inner fuel) to the constructor's result; the
-- conditional now picks ONLY the taken branch (`-then`/`-else`), as the direct
-- interpreter does.  Then `simF` is an induction on the OBJECT eval fuel `G`.
------------------------------------------------------------------------

khd-caseF : ∀ cp env e F {ve} →
  run (6 + F) (cp ∙ (⌜ e ⌝ ∙ env)) ≡ just ve →
  run (12 + F) (cp ∙ (⌜ khd e ⌝ ∙ env)) ≡ just (hd ve)
khd-caseF cp env e F he rewrite khd-dispatch cp env e F | he = refl

ktl-caseF : ∀ cp env e F {ve} →
  run (6 + F) (cp ∙ (⌜ e ⌝ ∙ env)) ≡ just ve →
  run (13 + F) (cp ∙ (⌜ ktl e ⌝ ∙ env)) ≡ just (tl ve)
ktl-caseF cp env e F he rewrite ktl-dispatch cp env e F | he = refl

kcons-caseF : ∀ cp env a b F {va vb} →
  run (7 + F) (cp ∙ (⌜ a ⌝ ∙ env)) ≡ just va →
  run (7 + F) (cp ∙ (⌜ b ⌝ ∙ env)) ≡ just vb →
  run (12 + F) (cp ∙ (⌜ kcons a b ⌝ ∙ env)) ≡ just (va ∙ vb)
kcons-caseF cp env a b F ha hb rewrite kcons-dispatch cp env a b F | ha | hb = refl

kifnil-thenF : ∀ cp env c t e F {v} →
  run (8 + F) (cp ∙ (⌜ c ⌝ ∙ env)) ≡ just ⟨⟩ →
  run (8 + F) (cp ∙ (⌜ t ⌝ ∙ env)) ≡ just v →
  run (16 + F) (cp ∙ (⌜ kifnil c t e ⌝ ∙ env)) ≡ just v
kifnil-thenF cp env c t e F hc ht rewrite kifnil-dispatch cp env c t e F | hc | ht = refl

kifnil-elseF : ∀ cp env c t e F {vh vt v} →
  run (8 + F) (cp ∙ (⌜ c ⌝ ∙ env)) ≡ just (vh ∙ vt) →
  run (8 + F) (cp ∙ (⌜ e ⌝ ∙ env)) ≡ just v →
  run (16 + F) (cp ∙ (⌜ kifnil c t e ⌝ ∙ env)) ≡ just v
kifnil-elseF cp env c t e F hc he rewrite kifnil-dispatch cp env c t e F | hc | he = refl

-- nothing ≠ just (the under-fuel / divergent sub-eval branches are absurd).
n≢j : ∀ {x : V} → nothing ≡ just x → ⊥
n≢j ()

-- common ≤ facts for the fuel bumps.
private
  ≤sum-l : ∀ k a b → a ≤ k + (a + b)
  ≤sum-l k a b = ≤-trans (m≤m+n a b) (m≤n+m (a + b) k)
  ≤sum-r : ∀ k a b → b ≤ k + (a + b)
  ≤sum-r k a b = ≤-trans (m≤n+m b a) (m≤n+m (a + b) k)

-- THE FULL G1: whenever the direct interpreter (program p, fuel G) computes `v`
-- on `e` in `env`, the self-interpreter computes the same value.
simF : ∀ G e {p env v} → Interp.eval p G e env ≡ just v →
       Σ ℕ (λ F → run F (⌜ p ⌝ ∙ (⌜ e ⌝ ∙ env)) ≡ just v)
simF zero    e        ()
simF (suc g) var       h = 8 , h
simF (suc g) knil      h = 8 , h
simF (suc g) (khd e) {p} {env} h with Interp.eval p g e env in eqe
... | nothing  = ⊥-elim (n≢j h)
... | just ve with simF g e eqe
...   | Fe , ihe = 12 + Fe ,
        trans (khd-caseF ⌜ p ⌝ env e Fe (mono-≤ (m≤n+m Fe 6) ihe)) h
simF (suc g) (ktl e) {p} {env} h with Interp.eval p g e env in eqe
... | nothing  = ⊥-elim (n≢j h)
... | just ve with simF g e eqe
...   | Fe , ihe = 13 + Fe ,
        trans (ktl-caseF ⌜ p ⌝ env e Fe (mono-≤ (m≤n+m Fe 6) ihe)) h
simF (suc g) (kcons a b) {p} {env} h with Interp.eval p g a env in eqa
... | nothing = ⊥-elim (n≢j h)
... | just va with Interp.eval p g b env in eqb
...   | nothing = ⊥-elim (n≢j h)
...   | just vb with simF g a eqa | simF g b eqb
...     | Fa , iha | Fb , ihb = 12 + (Fa + Fb) ,
          trans (kcons-caseF ⌜ p ⌝ env a b (Fa + Fb)
                   (mono-≤ (≤sum-l 7 Fa Fb) iha)
                   (mono-≤ (≤sum-r 7 Fa Fb) ihb)) h
simF (suc g) (kifnil c t e) {p} {env} h with Interp.eval p g c env in eqc
simF (suc g) (kifnil c t e) {p} {env} h | nothing = ⊥-elim (n≢j h)
simF (suc g) (kifnil c t e) {p} {env} h | just ⟨⟩ with simF g c eqc | simF g t h
... | Fc , ihc | Ft , iht = 16 + (Fc + Ft) ,
      kifnil-thenF ⌜ p ⌝ env c t e (Fc + Ft)
        (mono-≤ (≤sum-l 8 Fc Ft) ihc) (mono-≤ (≤sum-r 8 Fc Ft) iht)
simF (suc g) (kifnil c t e) {p} {env} h | just (vh ∙ vt) with simF g c eqc | simF g e h
... | Fc , ihc | Fe , ihe = 16 + (Fc + Fe) ,
      kifnil-elseF ⌜ p ⌝ env c t e (Fc + Fe)
        (mono-≤ (≤sum-l 8 Fc Fe) ihc) (mono-≤ (≤sum-r 8 Fc Fe) ihe)
simF (suc g) (call e) {p} {env} h with Interp.eval p g e env in eqe
... | nothing = ⊥-elim (n≢j h)
... | just arg with simF g e eqe | simF g p h
...   | Fe , ihe | Fp , ihp = 18 + (Fe + Fp) ,
        call-case ⌜ p ⌝ env e (Fe + Fp)
          (mono-≤ (≤sum-l 8 Fe Fp) ihe) (mono-≤ (≤sum-r 11 Fe Fp) ihp)

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
