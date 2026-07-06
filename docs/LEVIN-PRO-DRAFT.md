# Levin の最適探索定理の機械検証（PRO 投稿 通し原稿ドラフト）

本文全体の散文ドラフト（要旨＋§1〜§9）。である調・LaTeX 数式。数値/定理名は `proofs/agda`,
`proofs/rocq` で検証済み。§4 は要約を置き、詳細な書き下しは `LEVIN-PRO-DRAFT-S4.md` にある。
骨子と相互参照: `LEVIN-PRO-PAPER-SKELETON.md`、根拠: `LEVIN-PAPER-CONTRIBUTIONS.md`,
`RELATED-WORK.md`。**注記**: これはドラフトであり、投稿前に共著者確認・体裁調整・図の作成を要する。

---

## 題目
**Levin の最適探索定理の機械検証 — 解釈器オーバヘッド定数の定理化と可逆版への再利用**

## 要旨
最適探索は、反転問題に対して、最適なプログラムの定数倍の時間で解を見つける古典的なアルゴリズム
である。本論文では、Jones『Computability and Complexity』の定式化（定理 20.1.2）による Levin の
最適探索定理を、証明支援系で**初めて機械検証**した結果を報告する。証明は Agda（`--safe`）と
Rocq（9.1）の双方で、いずれも**公理を用いずに**与えられる。教科書の証明が存在のみを主張する
解釈器オーバヘッド定数を、具体的な自己解釈器に対して**定理 $a=73$ に格上げ**し（両系で同一値）、
その過程で「自己解釈器の効率の検証は正当性の検証をステップ計数へ精密化したものではない」という
方法論的知見を得た。最適性の核は**コストモデルに依存しない再利用可能な抽象定理**として定式化され、
同じ枠組みから、新規の**可逆版 Levin 最適性**（時間とガーベジのトレードオフ）が改変なしに導かれる。
機械検証した解釈器は OCaml へ抽出され、差分テストにより忠実性を実行的に確認した。定式化の
スコープ（1変数自己解釈器・オラクル検証器・ドライバ形の探索）は明示する。

## Abstract
Optimal search finds, for an inversion problem, a solution within a constant factor of the
time of the fastest program. We report the first machine-checked proof of Levin's optimal
search theorem in the formulation of Jones's *Computability and Complexity* (Thm 20.1.2).
The proof is carried out twice — in Agda (`--safe`) and Rocq (9.1) — and is axiom-free in
both. We turn the interpreter-overhead constant, which the textbook proof only asserts to
exist, into a concrete theorem $a=73$ (identical in both systems), and observe along the way
that verifying a self-interpreter's *efficiency* is not a step-accounting refinement of
verifying its *correctness*. The optimality core is a cost-model-agnostic, reusable
meta-theorem from which a novel reversible Levin optimality result (a time/garbage trade-off)
follows verbatim. The verified interpreter is extracted to OCaml and its faithfulness checked
executably by differential testing. We are explicit about the formalization's scope
(one-variable self-interpreter, oracle verifier, driver-form search).

---

## 1 はじめに

Levin の最適探索は、反転問題 — 与えられた $y$ に対して $f(x)=y$ なる $x$ を求める問題 — に対する
普遍的な探索戦略である。候補プログラムを Gödel 番号順に並べ、番号 $i$ のプログラムに予算 $2^{-i}$
の割合で時間を配分しながら**同時並行（dovetailing）**に走らせる。この戦略は、問題を解く**任意の**
プログラム $q$ に対して、$q$ 自身の実行時間 $T_q$ と検証時間 $T_r$ の定数倍
$a_q\,(T_q+T_r)$ の時間で解に到達する。定数 $a_q$ は $q$ に依存するが入力には依存しない。これが
Levin の最適性であり、本論文では Jones の教科書『Computability and Complexity』の定式化
（WHILE/I 言語による定理 20.1.2、および自己解釈器の効率に関する定理 19.1.3）を対象とする。この
定式化を選ぶ理由は、具体的なプログラミング言語・実行可能な自己解釈器・明示的なコストモデルの上で
議論が閉じており、機械検証に適するためである。

この定理は数学的にはよく知られているが、その証明には二つの、機械検証によって初めて顕在化する
論点がある。第一に、最適性の定数 $a_q$ は解釈器のオーバヘッド定数 $a$ を含むが、教科書はこの $a$ の
**存在を主張するだけで具体値を与えない**。第二に、我々の調査（第8節）の限りでは、この定理は
**いかなる証明支援系でも機械検証されていない**。最近傍は Kolmogorov 複雑性の形式化であり、Levin の
最適探索そのものの機械化は見当たらない。

本論文の貢献は次のとおりである。

- **C1**：Levin の最適探索定理（定理 20.1.2）の初の機械検証。Agda（`--safe`）と Rocq（9.1）の
  二重検証であり、いずれも公理ゼロである（§3・§4・§5）。
- **C2**：解釈器オーバヘッド定数の**定理化** $a=73$。両系で同一値であり、これを定理とするために
  必要なコストモデル・解釈器・課金方式のモデリング確約を明確化する（§4）。
- **C3**：方法論的知見。**`refl` による dispatch は自己解釈器の正当性を機械化できるが、コスト（歩数）
  には使えず**、fuel を用いない大ステップのコスト関係を要する（§4）。
- **C4**：**コストモデル非依存の再利用可能な枠組み**。最適性の核（Layer A）は抽象定数上の
  メタ定理であり、新規の**可逆版 Levin**（時間とガーベジのトレードオフ定理）が同じ枠組みを
  改変なしにインスタンス化して得られる（§6）。
- **C5**：抽出した検証済み解釈器の差分テストによる忠実性検証と、Agda・Rocq の機械化上の知見（§5）。

あわせて、本形式化のスコープを前もって明示しておく。証明したのは、1変数 I 自己解釈器の per-node
単位コストのもとで線形オーバヘッド $a=73$ を持つ、dovetailing ドライバに対する Levin 最適性であり、
検証器は自由なオラクルとしてモデル化する。この限界の詳細と、それが定理の主張に与える精確な形は
第7節で述べる。**なお、機械検証の結果として、Jones の証明そのものには欠落や誤りは見出されなかった**
——教科書の証明が忠実であることを確認したこと自体、正直な結果として報告する。

## 2 準備

**値と言語**　値 $V$ は二分木（`cons` 木、葉は $\langle\rangle$）である。被解釈言語は1変数の I 言語
$\mathrm{Exp}$ であり、構成子は `var`（唯一の変数）, `knil`（$\langle\rangle$）, `kcons`,
`khd`, `ktl`（対の構成・射影）, `kifnil`（$\langle\rangle$ 判定による条件分岐）, `call`
（再帰呼出し）である。意味論は燃料付きの評価器 $\mathrm{eval} : \mathbb{N}\to\mathrm{Exp}\to V\to
\mathrm{Maybe}\,V$ で与える（`UniversalInterp.agda`）。

**コストモデル**　1ノードの評価を1単位とし、部分評価のコストを総和する per-node 単位コスト
（`StepCount.evalT`）を採る。これは本書の $\mathit{time}_p$ および実装の `while --time` と一致する。
大ステップの形では、式 $e$ が環境で値 $v$ に $c$ ステップで評価されることを
$e,\mathit{env}\Downarrow v\#c$ と書く（`CostRelation`）。

**自己解釈器と符号化**　プログラムの符号化 $\ulcorner p\urcorner \in V$（`ProgEncoding`）を用い、
自己解釈器 $\mathit{sint}\in\mathrm{Exp}$（`SelfInterp`）は $\ulcorner p\urcorner$ とデータを組にした
入力に対し $p$ の意味を計算する。

**機械検証の意味**　Agda 側は `--safe`（`TERMINATING`/`trustMe` を禁止し、健全性を保証する）で
チェックし、`postulate` を一切用いない。Rocq 側は `Print Assumptions` が "Closed under the global
context" を返す（公理ゼロ）。以降「機械検証」「証明した」はこの意味で用いる。

## 3 最適性の抽象定理（Layer A）

最適性の証明を、解釈器から独立な**抽象メタ定理**として定式化する（`LevinOptimal.agda`）。まず
dovetailing の予算算術を用意する。位相 $0..k$ で費やす総予算は幾何和
$$ \mathrm{gsum}\,k \;=\; \sum_{i=0}^{k} 2^{i} \;=\; 2^{\,k+1}-1 $$
であり（`LevinBudget.gsum`、`gsum-eq : \mathrm{suc}(\mathrm{gsum}\,k) \equiv 2^{k+1}$）、幾何和の
上界 $\mathrm{gsum}\,k < 2\cdot 2^{k}$ と、目標を超える最小の2冪に関する overshoot 上界
（最小の $2^{k}\ge N$ は $\le 2N$）が鍵となる。プログラムの Gödel 番号付けには Cantor の対関数
$\mathrm{pair}\,x\,y = \mathrm{tri}(x+y)+x$（`NatPairing`、単射）を用いる。その逆 $\mathrm{unpair}$
は対角線walkの後者再帰により**構造的再帰**で定義でき、整礎再帰も燃料も要しない。

主定理は次の形をとる（`LevinOptimal.levin-optimal`）。探索の定数を
$$ a_q \;=\; 4\,a\,2^{j} $$
とおくと（実装では `aq = (c+c)+(c+c) = 4c`、$c = a\,2^{j}$）、正値条件 $1\le c\cdot S$ のもとで
$$ \exists\,k.\quad \mathrm{gsum}\,k \;<\; a_q\cdot S $$
が成り立つ。ここで係数 $4=2\times 2$ は、幾何和の上界に由来する $2$ と overshoot に由来する $2$ の積
であり、$2^{j}$ は倍々スケジュール（番号 $j$ のプログラムが位相 $j$ で初めて予算を受ける）に由来する。
この定理は overhead を抽象的な $\mathbb{N}$ 値フィールド（`aq`, `c`, `S`）のみに依存して証明されて
おり、**具体的な解釈器には一切依存しない**。この抽象性が第6節の再利用を可能にする。

探索が実際に停止する位相 $k_{op}$ は、最適プログラムが最初に見つかる位相 $k$ に対して
$k_{op}\le k$ でよい（より速いプログラムが先に検証を通しうる）。`gsum` の単調性により最適性の
束縛はそのまま保たれる（`LevinComplete.levin-operational`）。

## 4 解釈器オーバヘッド定数の定理化 $a=73$

（本節は要約。詳細な書き下しは `LEVIN-PRO-DRAFT-S4.md` を参照。）

第3節の $a_q=4\,a\,2^{j}$ に含まれる $a$ は、自己解釈器が被解釈プログラムの1ステップを模倣する際の
線形オーバヘッド係数であり、Jones 定理 19.1.3 はその存在のみを主張する。本節はこれを1変数 I 自己
解釈器 $\mathit{sint}$ に対し具体的に計算し、$a=73$ を定理として確定する。$73$ は各構成子の dispatch
コスト $\{$`var` 8, `knil` 12, `khd` 39, `ktl` 48, `kcons` 45, `kifnil` 73, `call` 64$\}$ の**最大値**
であり（`SintCost`, `IntEfficient`）、最大を与えるのは条件ノード `kifnil` である。

方法論上の要点は、自己解釈器の**正当性**が各 dispatch を `refl`（定義計算）で閉じるのに対し、
**コスト**は同じ手法の精密化では得られないことである。部分評価の歩数が記号的に stuck した自己
解釈の内側に凍結され、総和の形に取り出せない。そこで fuel を用いない大ステップコスト関係
$e,\mathit{env}\Downarrow v\#c$ を導入し、部分計算を不透明な前提として扱う。この関係への帰納により、
$$ e,\mathit{env}\Downarrow v\#c \;\Longrightarrow\; \exists\,c'\le 73\,c.\ \ \mathit{sint}\ \text{が}\ v\ \text{を}\ c'\ \text{ステップで再現} $$
を証明する（`int-efficient`）。各ノード不等式 $K+\sum_i c_i' \le a(1+\sum_i c_i)$ は $K\le a=\max K$
から従う。得られた定数を Layer A に戻すと、`overhead-grounded : \mathrm{LinearOverhead}\,73 =
\mathtt{int\text{-}efficient}$ により overhead 仮定が除去され、最適性は $a_q=4\cdot73\cdot2^{j}$ の
形で、$a$ を仮定でなく**定理**として含む（`LevinGrounded`）。同じ $73$ は Rocq でも独立に再証明される。

## 5 二重検証と抽出

**二重検証**　Levin の連鎖は Agda（`--safe`、83 モジュール）と Rocq（9.1、その 22 モジュールの双子）
で二度証明され、いずれも公理ゼロである。同一定理を二系で再証明したことで、単一処理系に固有の
アーティファクトである可能性が排除される。加えて、二つの機械化を比較すると、機械検証上の知見が
一つの実質的な**分岐**と幾つかの**収束**として得られる。

唯一の実質的な分岐は、自己解釈器の per-node dispatch 等式にある。Agda ではこれが定義計算により
`refl` で閉じるが、Rocq のカーネル `reflexivity` は、二つ・三つの部分実行が代入された段階で
**指数的に爆発**する（`kcons` で約 100 秒、`kifnil` で 300 秒超）。Rocq 側はこれを `vm_compute` と、
stuck した部分実行への `match` を反映した**形状忠実な右辺**で回避する（`SimF.v`, `DispCall.v`）。逆に、
具体的なプログラム全体の実行では Rocq は素の `reflexivity` を用いる（`vm_compute` が爆発するため）。
一方、より本質的な構造的問いは両系で**同一に**解決した。Cantor の $\mathrm{unpair}$ は構造的再帰で
書け、$\mathrm{vnat}$ の逆のみが燃料を要し（返り値が構造的に小さくならず、番号そのものを燃料とする）、
線形オーバヘッド束縛は部分実行を不透明に保つために大ステップコスト関係を要する。そして両系が
到達する定数はいずれも $a=73$ である。

**抽出と差分テスト**　機械検証した Rocq の解釈器は OCaml へ抽出される（`Extract.v`→`while_eval.ml`：
検証済みの $\mathrm{run}$/$\mathit{sint}$/$\mathrm{enc}$/$\mathrm{decode}$）。`test-extract.sh` は、
抽出した検証済み解釈器を OCaml として走らせ、全 `Exp` 構成子・再帰的なリストプログラム（長さ・連結・
反転）・自己解釈・実行時の $G1$（プログラムの直接実行とその自己解釈が一致）を、証明済みのオラクル値と
照合する。さらに `test-oracle.sh` は $\ulcorner p\urcorner$ 解釈器 $\mathrm{rundata}$ を参照実装
$\mathrm{EvalWhile}$ に対し二つの符号化で突き合わせ、`test-cogen.sh` は擬似乱数入力による差分テストを
加える。これらは一つの `run-all.sh`（約 24 スクリプト）に統合されている。すなわち、証明は記号だけの
存在ではなく、**動いて参照実装と一致する**ことが実行的に確かめられている。

## 6 再利用可能枠組みと可逆版 Levin

第3節の Layer A が具体解釈器に依存しないことの帰結として、最適性の枠組みは**再利用可能**である。
この再利用は主張ではなく検証されている。可逆計算に対する新規の Levin 最適性を扱うモジュール
（`RevLevinAbstract`, `RevLevinGrounded`, `RevLevin`）は、古典の Layer A を
`open import LevinOptimal using (LevinSearch; levin-optimal; aq; c; S)` として**改変なしに**
インスタンス化する。dovetailing の上部構造（`LevinBudget`, `NatPairing`, `LevinOptimal`,
`LevinDovetail`, `LevinComplete`）はそのまま再発火する。

この再利用が生む結果は新規である。可逆版 Levin 最適性は、時間最適性（証明された可逆オーバヘッド
定数のもとでの $\mathrm{gsum}\,k < a_q\cdot S$）と、**ガーベジの挟み撃ち**（$|\mathrm{fibre}| \le g
\le |\mathrm{input}|$）を同時に主張する、時間とガーベジの**トレードオフ定理**である
（`RevLevinMain`）。可逆自己解釈器は実際の可逆機械 $\mathit{rsint}$ に接地されており
（`RevRsint`：$\mathit{rsint}$ が前向きに計算し（`rsint-computes`）、全体として可逆である
（`rsint-reversible`）。done スタックがまさに Bennett ガーベジである）、これも公理ゼロで機械検証
されている。したがって古典 Levin の形式化は「既知定理を機械化しただけ」ではなく、**新しい定理を
生む再利用可能な土台**として機能する。可逆版の詳細は別稿の主題としうるが、本論文では枠組みの
再利用が非自明な新結果に至ることを示す例として提示する。

## 7 限界と今後

本形式化のスコープと限界を明示する。これらは Jones の証明の欠陥ではなく、**形式化の**限界であり、
定理の主張を精確にするために述べる。

第一に、$a=73$ は**1変数 I 自己解釈器** $\mathit{sint}$ の per-node 単位コストにおける定数であって、
多変数 WHILE の万能プログラムのオーバヘッドではない。後者は変数リスト長に比例して増大し、実測では
比 $\approx 2350$（$O(\#\mathrm{vars})$）に達する（`docs/EFFICIENCY.md`）。WHILE から I への
翻訳コストは本形式化の対象外である。

第二に、検証器 $r$ は step 課金していない。検証器は自由な意味論的オラクル $\mathrm{chk}:V\to V\to
\mathrm{Bool}$ としてモデル化され、実際に証明した束縛は $\mathrm{gsum}\,k < a_q\cdot T_q$ であって、
$T_q\le T_q+T_r$ の padding により $a_q\,(T_q+T_r)$ の形に弱めている。$r$ を $\mathit{sint}$ 経由で
課金する（予算 $\ge 73\,(T_q+T_r)$）ことは今後の課題である。

第三に、探索本体 $\mathit{opt}$ は Agda のメタドライバであり、リテラルな $\mathrm{Exp}$ プログラムでは
ない（制御ループと予算倍化は `LevinOptE`/`LevinBudgetE` で部分的に構築済み）。証明はバッチコスト
モデルのもとでの dovetailing ドライバに関する定理である。

以上より、本論文が確立した定理は次のように述べるのが正確である。**「1変数 I 自己解釈器 $\mathit{sint}$
の per-node 単位コストのもとで線形オーバヘッド $a=73$ を（二重に）証明し、検証器を自由オラクルと
した dovetailing ドライバに対する Levin 最適性」**。今後の課題は、リテラルな $\mathit{opt}\in
\mathrm{Exp}$ の完成、検証器 $r$ の課金、多変数オーバヘッドの定理化、および Rocq 側への可逆連鎖の移植
である。

## 8 関連研究

**先行の機械化**　我々の文献調査（2026年、Semantic Scholar・Crossref・arXiv・Isabelle AFP・Lean
mathlib）の限りでは、Levin の最適探索そのものの機械化は存在しない。最近傍は Kolmogorov 複雑性の
形式化である。Forster, Kunze, Lauermann は Kolmogorov 複雑性を Coq で**合成的（synthetic）**に、
すなわち計算モデルを固定せず普遍関数を抽象的に仮定して形式化し、議論の構成性を強調した（ITP 2022）。
本論文はこれと相補的に、**具体的**な立場をとる。Jones の実際の WHILE 自己解釈器を実体化し、合成的
アプローチが抽象化する当の定数を証明する。彼らの構成性の強調に対応する本論文の格上げ物は**公理
ゼロ**と**二重検証**である。Catt と Norrish は Kolmogorov 複雑性を HOL4 で形式化した（CPP 2021）が、
対象は複雑性であって最適探索ではなく、単一処理系である。これらは、Kolmogorov/Levin 近傍の古典的
結果の忠実な機械化が CPP/ITP といった最上位の形式化会議で貢献と認められている**先例**を与える。

**紛らわしい別定理**　Isabelle AFP には `Cook_Levin`（SAT の NP 完全性、Cook–Levin の定理）が
あるが、これは Levin の**最適探索とは別の定理**であり、混同してはならない。

**部分評価・Futamura 射影の形式化**　本研究の周辺（自己解釈器・特殊化）に関わる形式化として、Latifi
による第二 Futamura 射影の実装（SPLASH 2019）や、可逆側で近い Glück と Normann の部分評価による反転
の実験（EPTCS 2024）がある。

**アルゴリズムとしての Levin 探索**　Levin 探索は AI の文脈でアルゴリズムとしては現役であり
（Levin Tree Search、Orseau・Hutter ら、IJCAI 2023 等）、種々の変種が研究されているが、証明支援系
での検証は皆無である。

## 9 おわりに

本論文は、Jones の定式化による Levin の最適探索定理を、証明支援系で初めて、Agda と Rocq の双方で
公理を用いずに機械検証した。教科書が存在のみを主張する解釈器オーバヘッド定数を具体的な定理 $a=73$ に
格上げし、その過程で「自己解釈器の効率の検証は正当性の検証の精密化ではない」という方法論的知見を得た。
最適性の核はコストモデル非依存の再利用可能な枠組みであり、そこから新規の可逆版 Levin 最適性が改変
なしに導かれる。機械検証した解釈器の抽出と差分テストにより忠実性を実行的に確認した。同時に、機械
検証は Jones の数学的議論に欠陥を見出さず、教科書の証明が忠実であることを確認した。本形式化の
スコープ（1変数自己解釈器・オラクル検証器・ドライバ形の探索）は明示し、その拡張を今後の課題とする。

## 参考文献
`LEVIN-PRO-PAPER-SKELETON.md` の参考文献節（裏取り済み、`RELATED-WORK.md` と整合）を参照。主要:
Levin 1973; Jones 1997（定理 19.1.3, 20.1.2）; Forster, Kunze, Lauermann, ITP 2022; Catt, Norrish,
CPP 2021; Latifi, SPLASH 2019; Glück, Normann, EPTCS 413, 2024; Orseau, Hutter et al., IJCAI 2023。
