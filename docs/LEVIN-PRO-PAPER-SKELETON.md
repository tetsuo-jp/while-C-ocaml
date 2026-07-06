# PRO 投稿用 本文骨子 — Levin 最適探索定理の機械検証

情報処理学会論文誌 プログラミング(PRO)向けの本文骨子。各節に「書くこと・主張・引用す
る成果物(モジュール)・目安分量」を箇条書きで置く。散文化は執筆時。根拠は
`docs/LEVIN-PAPER-CONTRIBUTIONS.md`(①〜⑥)・`docs/RELATED-WORK.md`(Levin節)・
`docs/LEVIN-PLAN.md`。**正直性方針**：穴は捏造しない／3つの限界(§7)を明示する。

- **言語**: 日本語 or 英語どちらでも可(PRO 両対応)。形式化論文は英語も選択肢。本骨子は
  日本語見出し＋英語術語で記述。
- **分量目安**: 全体 12–16 頁(PRO 標準)。各節の目安を併記。

---

## 題目候補
1. 「Levin の最適探索定理の機械検証 — 解釈器オーバヘッド定数の定理化と可逆版への再利用」
2. (EN) *Machine-Checking Levin's Optimal Search: Turning the Interpreter-Overhead
   Constant into a Theorem, Twice*
3. (EN) *A Doubly-Verified, Reusable Formalization of Levin's Optimal Search*

→ 推奨: 1(和) / 2(英)。「二重検証」「定数の定理化」「再利用(→可逆)」の3点が題目に出る形。

## 要旨(Abstract) 骨子 〜200字/150 words
- Levin の最適探索(Jones『Computability and Complexity』Thm 20.1.2)を **初めて機械検証**。
- Agda(`--safe`) と Rocq(9.1) で **二重に、公理ゼロで** 証明。
- 教科書が存在だけ主張する解釈器オーバヘッド定数を **定理 `a=73` に格上げ**(両系で一致)。
- 最適性の核(Layer A)は **コストモデル非依存の再利用可能枠組み** で、**新規の可逆版 Levin**
  (時間↔ガーベジのトレードオフ)を同じ枠組みからそのまま導ける。
- 抽出した検証済み解釈器の差分テストで忠実性を実行的に確認。限界(1変数 sint 定数・検証器
  はオラクル・opt はドライバ)を明示。

## Contributions(§1 末に箇条書きで明示)
- **C1**: Levin 最適探索定理の初の機械検証(Agda `--safe` ＋ Rocq、公理ゼロ)。§0/§3–4
- **C2**: 解釈器オーバヘッド定数の **定理化** `a=73`(両系一致)＋モデリング確約の明確化。§4
- **C3**: 方法論的知見 — **`refl`-dispatch は自己解釈器の*正当性*を機械化できるが*コスト*
  はできない**(big-step コスト関係が必要)。§4
- **C4**: **再利用可能なコストモデル非依存枠組み**。同じ Layer A が新規の **可逆版 Levin**
  (トレードオフ定理)を verbatim にインスタンス化。§6
- **C5**: 抽出＋差分テストによる忠実性の実行的検証、Agda vs Rocq の機械化知見。§5
- (C1 が背骨、C2–C3 が実質差分、C4 が payoff、C5 が頑健性。)

---

## §1 はじめに(Introduction) 〜2頁
- Levin 最適探索とは: 反転問題に対する dovetailing 探索。最適プログラムの定数倍で走る。
  Jones 20章の定式化(WHILE/I 言語、Thm 20.1.2)を採る理由(具体的・教科書的・実行可能)。
- 問題意識: (i) 証明は解釈器オーバヘッド定数 `a` の存在を **仮定するだけ** で計算しない;
  (ii) この定理は **どの証明支援系でも機械化されていない**(§8 で裏取り)。
- 本論文: 初の機械検証。**定数を定理化**し、**二重検証**し、**再利用枠組み**として可逆版を導く。
- 正直な立ち位置を前倒しで一文: 「1変数 I 自己解釈器・per-node 単位コスト・検証器はオラクル
  ・opt はドライバ」というスコープで、Levin 最適性を厳密化した(詳細 §7)。
- Contributions(上の C1–C5)。

## §2 準備(Preliminaries) 〜2頁
- 値 `V`(cons木)、I 言語 `Exp`(var/knil/kcons/khd/ktl/kifnil/call)、意味論
  `eval : ℕ→Exp→V→Maybe V`(fuel付)。引用: `UniversalInterp`。
- 単位コストモデル `time_p`(1 node = 1 step、`while --time` に一致)。引用: `StepCount.evalT`。
- 自己解釈器 `sint : Exp` と ⌜p⌝ エンコード。引用: `SelfInterp`, `ProgEncoding`。
- Jones Thm 20.1.2 の非形式的主張。dovetail スケジュール、最適性 `a_q·(T_q+T_r)`。
- 機械検証の意味: Agda `--safe`(TERMINATING/trustMe 禁止・公理ゼロ)、Rocq `Print
  Assumptions`=Closed。

## §3 最適性の抽象定理(Layer A) 〜2頁
- 算術基盤: `LevinBudget`(幾何和 `gsum`, `gsum<2last`, `overshoot`)、`NatPairing`
  (Cantor pairing、unpair は **構造的再帰**)。
- **Layer A の主定理** `LevinOptimal.levin-optimal`: 最適性不等式を **抽象メタ定理** として証明。
  overhead は抽象 `ℕ` フィールド(`aq`,`c`,`S`)のみに依存 — 具体解釈器に非依存。
- 定数の形 `a_q = 4·a·2^j` を導出し **`4=2×2` を分解**(幾何和の2 × overshoot の2)、`2^j` は
  倍々スケジュール。§①-3 の内容。
- 「この抽象化が §6 の再利用を可能にする」と伏線。

## §4 解釈器オーバヘッド定数の定理化 `a=73` 〜3頁(キー節)
- `sint` の per-node dispatch コスト。`SintCost`: 7定数 {var 8, knil 12, khd 39, ktl 48,
  kcons 45, kifnil 73, call 64}、`a = 73 = max`。
- **C3(方法論)**: 正当性(`SelfInterpCorrect.simF`)は各 node が `refl` で閉じるが、**step count
  は refl で閉じない**(部分実行の count が記号的自己解釈の中に凍結)。→ **fuel-free の
  big-step コスト関係** `CostRelation._,_⇓_#_` に切替、部分実行を **不透明な前提** として入れる。
  これが `IntEfficient` の構造の理由。
- `IntEfficient.int-efficient`: `usintC n ⌜c⌝ s ≤ a·ocost`(a=73)。オブジェクト導出への帰納。
- `LevinGrounded`: `a=73` を Layer A に投入し overhead 仮定を **除去**(`overhead-grounded`)。
- **モデリング確約(C2)**: (i) 単位コストモデル、(ii) 1変数 I `sint`、(iii) per-node 一様課金。

## §5 二重検証と抽出(頑健性 C5) 〜2頁
- **Agda vs Rocq 分岐表**(§⑤): 唯一の実質分岐 = per-node dispatch。Agda は `refl`(定義的)、
  Rocq は `vm_compute`＋shape-faithful RHS(kernel `reflexivity` は multi-sub で指数爆発、
  kcons~100s/kifnil>300s; `SimF.v`/`DispCall.v`)。具体全実行は Rocq は逆に `reflexivity`。
- 収束(両系一致): Cantor unpair は構造的、`vunnat` は fuel、int-efficient は big-step 関係、
  定数 `a=73`。→「同一定理を2系で再証明し、難所が同じ所に出た」= 頑健性の証拠。
- **抽出＋差分テスト(④)**: `Extract.v`→`while_eval.ml`(検証済み `run`/`sint`/`enc`/`decode`)。
  `test-extract.sh` が全 `Exp` 構成子・再帰リストプログラム・自己解釈・G1 を実行時照合;
  `test-oracle.sh` が `rundata` vs `EvalWhile` を両エンコードで; `test-cogen.sh` は fuzz 付。
  → 「証明は記号だけでなく動いて現実と一致」。

## §6 再利用枠組み → 可逆版 Levin(payoff, C4) 〜2頁
- **同じ Layer A を verbatim 再利用**: `RevLevinAbstract`/`RevLevinGrounded`/`RevLevin` が
  `open import LevinOptimal using (LevinSearch; levin-optimal; aq; c; S)` — Layer A を無改変で
  インスタンス化(**主張でなく検証**)。
- **新規の結果**: 可逆版 Levin 最適性 + **時間↔ガーベジのトレードオフ**(`RevLevinMain`:
  proven 定数で時間最適 AND ガーベジ `|fibre| ≤ g ≤ |input|`)、実際の可逆機械 `rsint`
  (`RevRsint`: `rsint-computes`/`rsint-reversible`、done-stack = Bennett ガーベジ)に接地。
- 位置づけ: 「古典 Levin の機械化は再利用可能枠組みで、その最初の非自明な再利用が
  *新定理* を生む」= rote formalization 批判への回答。(可逆版は novel、別途詳細も可)。

## §7 限界と今後(Limitations & Future Work) 〜1頁 【必須・査読対応の要】
- **要開示 caveat 3点**(§① より):
  1. `a=73` は **1変数 I `sint`** の定数(per-node 単位コスト)。**多変数 WHILE universal の
     オーバヘッドではない**(`EFFICIENCY.md` 実測 ≈2350、`O(#vars)`)。WHILE→I 変換コストは未機械化。
  2. **検証器 `r` は step 課金していない**(オラクル `chk`)。証明した束縛は実質 `gsum k < a_q·T_q`
     で、`T_q ≤ T_q+T_r` の padding で `a_q·(T_q+T_r)` に弱めている。`r` を `sint` 経由で課金するのは今後。
  3. **`opt` は Agda メタドライバ**で、リテラルな `Exp` プログラムではない(`LevinOptE`/
     `LevinBudgetE` で部分構築)。バッチコストモデル下のドライバの定理。
- **正直な定理文**: 「dovetailing ドライバに対する Levin 最適性。1変数 I `sint` の per-node 単位
  コストで線形オーバヘッド `a=73` を(二重)証明、検証器は自由オラクル」。
- 今後: リテラル `opt∈Exp`、`r` の課金、多変数オーバヘッドの定理化、Rocq 側の可逆チェーン移植。

## §8 関連研究(Related Work) 〜1.5頁
- **文献調査(2026-07)**: Levin 最適探索の先行機械化は **存在せず**(S2/Crossref/arXiv/AFP/mathlib)。
- **最近傍 = Kolmogorov 複雑性の形式化2本**、その論法を鏡写しに:
  - Forster, Kunze, Lauermann 2022(ITP): synthetic KC(普遍関数を*仮定*、constructive 強調)。
    → 我々は逆に **具体**(実 WHILE `sint`)で、彼らが抽象化する定数を **証明**。彼らの
    constructiveness に対応する我々の格上げ物 = **公理ゼロ**＋**二重検証**。
  - Catt, Norrish 2021(CPP): KC の形式化(HOL4)。→ 対象は複雑性で探索でない・単一系。
    「KC 隣接の忠実な機械化は CPP/ITP で貢献と認められる」= 我々の先例。
- **重要な区別**: Isabelle AFP `Cook_Levin` は **SAT の NP完全性**(別の Levin 定理)。混同注意。
- **PE/Futamura 形式化**(本リポの周辺): Latifi 2019、Glück ら; 可逆側で近い
  Glück & Normann 2024。
- Levin 探索は AI で **アルゴリズム** としては現役(Levin Tree Search, Orseau & Hutter 2023)
  だが、証明支援系での検証は皆無。

## §9 おわりに(Conclusion) 〜0.5頁
- 初の機械検証 + 定数の定理化 + 二重検証 + 再利用(→可逆) を要約。
- 「教科書証明は健全と機械検証で確認、その上で informal な核(定数)を厳密化し、枠組みは
  新定理を生む」。

## 参考文献(裏取り済み、`RELATED-WORK.md` と一致)
- Levin, L.A. 1973. *Universal sequential search problems.* Problemy Peredachi Informatsii 9(3).
- Jones, N.D. 1997. *Computability and Complexity: From a Programming Perspective.* MIT Press.(Thm 19.1.3, 20.1.2)
- Forster, Y., Kunze, F., Lauermann, N. 2022. *Synthetic Kolmogorov Complexity in Coq.*
  ITP 2022, LIPIcs vol. 237, 12:1–12:19. doi:10.4230/LIPIcs.ITP.2022.12
- Catt, E., Norrish, M. 2021. *On the formalisation of Kolmogorov complexity.*
  CPP 2021, pp. 291–299. doi:10.1145/3437992.3439921
- Latifi, F. 2019. *Practical second Futamura projection…* SPLASH Companion 2019,
  pp. 29–31. doi:10.1145/3359061.3361077
- Glück, R., Normann, L.M. 2024. *Inversion by Partial Evaluation: A Reversible
  Interpreter Experiment.* EPTCS vol. 413, pp. 1–14. doi:10.4204/EPTCS.413.1
- Orseau, L., Hutter, M., et al. 2023. *Levin Tree Search with Context Models.* IJCAI 2023.
- (自リポ成果物: Agda `proofs/agda`(`--safe`)、Rocq `proofs/rocq`(9.1)。公開 URL は投稿時に。)

---

## 査読対応メモ(anticipated reviewer concerns → どこで先回りするか)
| 想定懸念 | 先回り |
|---|---|
| 「既知定理の再形式化では?」 | §6(再利用→新定理) + §4(C2/C3) を実質差分として前面。§8 で KC 論文の先例。 |
| 「`a=73` は実装依存では?」 | §4 でモデリング確約を明示、§7-1 で 1変数 sint 限定と ≈2350 を正直に開示。 |
| 「`T_r` は本当に効いているのか?」 | §7-2 で「スラックのみ・今後」と明記(隠さない)。 |
| 「`opt` は本物のプログラムか?」 | §7-3 でドライバと明記、部分構築の現状を示す。 |
| 「単一処理系の産物では?」 | §5 の二重検証・分岐表で反論。 |
| 「Cook–Levin と同じでは?」 | §8 で SAT NP完全性は別定理と明示。 |

## 投稿先・形態
- **PRO(プログラミング研究会論文誌)** を第一候補。C1(初の機械化)が背骨、C4(可逆 payoff)で
  novelty を上乗せ。verified pearl 寄りの書き方も可。
- 可逆版を主役にするなら別稿(より novel)。本稿は「古典 Levin の機械化＋再利用の実証」を主軸に、
  可逆版は §6 で payoff として提示(全体を1本にまとめる構成)。
