# §4 解釈器オーバヘッド定数の定理化（散文ドラフト）

PRO 本文 §4 の書き下し。数式は LaTeX 記法で示す（組版時にそのまま流用可）。数値・定理名は
`proofs/agda`（`SintCost.agda`, `IntEfficient.agda`, `CostRelation.agda`,
`LevinGrounded.agda`）で検証済み。である調。骨子は `LEVIN-PRO-PAPER-SKELETON.md` §4。

---

## 4 解釈器オーバヘッドの定数 $a=73$

### 4.1 Jones の存在主張を具体的な定数にする

第3節で示した最適性の抽象定理（Layer A）は、探索の定数 $a_q = 4\,a\,2^{j}$ を、解釈器の
オーバヘッド定数 $a$ に対して相対的に与える。ここで $a$ は「効率的な自己解釈器が被解釈
プログラムの1ステップを高々 $a$ 実ステップで模倣する」という線形オーバヘッドの係数であり、
Jones の定理 19.1.3 はその**存在のみ**を主張して具体値を与えない。本節では、この $a$ を実際の
自己解釈器に対して**計算し、定理として確定する**。結果は $a = 73$ であり、後述のとおり Agda と
Rocq の双方で同一の値として機械検証される。

対象とする自己解釈器は、1変数の I 言語で書かれた自己解釈器 $\mathit{sint} \in \mathrm{Exp}$
（`SelfInterp.agda`）である。$\mathit{sint}$ は、被解釈プログラムの符号化 $\ulcorner p\urcorner$
と式・環境の符号化を組にした入力 $\ulcorner p\urcorner \cdot (\ulcorner e\urcorner \cdot
\mathit{env})$ を受け取り、プログラム $p$ の式 $e$ を環境 $\mathit{env}$ の下で評価した結果を
再現する。コストモデルは、I 言語の1ノードの評価を1単位とし、部分評価のコストを総和する
**per-node 単位コスト**（`StepCount.evalT`）を採る。これは本書の $\mathit{time}_p$ および実装の
`while --time` と一致する。

$\mathit{sint}$ が被解釈式の各構成子を処理する際に費やす自身のステップ数は、構成子ごとに定数で
上から押さえられる。実際に機械検証した各ノードの dispatch コスト（`SintCost.agda`,
`IntEfficient.agda`）は次のとおりである。

| 構成子 | `var` | `knil` | `khd` | `ktl` | `kcons` | `kifnil` | `call` |
|---|---|---|---|---|---|---|---|
| dispatch コスト $K$ | 8 | 12 | 39 | 48 | 45 | 73 | 64 |

線形オーバヘッド定数は、これら7定数の最大値
$$ a \;=\; \max\{8,12,39,48,45,73,64\} \;=\; 73 $$
であり（`IntEfficient.a = 73`）、最大を与えるのは条件分岐ノード `kifnil` である。$\mathit{sint}$
はタグ木を `kifnil` の入れ子で dispatch するため、条件ノードに到達するまでの分岐walkが最も深い。

### 4.2 正当性は `refl` で閉じるが、コストは閉じない（方法論的知見）

自己解釈器の**正当性**（$\mathit{sint}$ が被解釈式と同じ値を計算すること）の証明では、各構成子の
dispatch 補題が**定義計算のみ（`refl`）で閉じる**（`SelfInterpCorrect.agda`。分岐の定数は少量の
fuel グリッド探索で得た）。しかし、本節が対象とする**コスト**（ステップ数）については、同じ
`refl`-dispatch を精密化して歩数を数え上げる素朴な方針は**破綻する**。理由は、部分評価の歩数が、
記号的に stuck した自己解釈の内側に凍結されてしまい、総和 $K + \sum_i c_i'$ の形に取り出せない
ことにある。すなわち、**解釈器の効率の検証は、正当性の検証をステップ計数へ精密化したものでは
ない**。これは形式化を通じて顕在化した方法論的知見である。

本形式化ではこの障害を、fuel を用いない**大ステップのコスト関係**
$$ e ,\ \mathit{env} \ \Downarrow\ v \ \#\ c $$
（`CostRelation._,_⇓_#_`。式 $e$ が環境 $\mathit{env}$ で値 $v$ に $c$ ステップで評価される、の意）
を導入して回避する。この関係のもとでは、部分計算は規則の**不透明な前提**として入り、その内部
歩数を記号的に展開する必要がない。$\mathit{sint}$ のコスト補題（`sint-var`, `sint-knil`,
`sint-khd`, …, `sint-call`）は、被解釈式の1ノードの評価を、この関係を用いて
「$K$ ＋ 部分評価のコスト」の形に関係づける。この設計が、次項の主定理 `int-efficient` の構造を
規定している。

### 4.3 線形オーバヘッド定理

主定理は次の形をとる（`IntEfficient.int-efficient`）。被解釈式 $e$ がオブジェクト意味論で
$e , \mathit{env} \Downarrow v \# c$ と評価されるとき、$\mathit{sint}$ は同じ値 $v$ を高々
$a \cdot c = 73\,c$ ステップで再現する。すなわち
$$ e ,\ \mathit{env} \ \Downarrow\ v \ \#\ c
   \;\Longrightarrow\;
   \exists\,c' \le 73\,c.\quad
   \mathit{sint},\ (\ulcorner p\urcorner \cdot (\ulcorner e\urcorner \cdot \mathit{env}))
   \ \Downarrow\ v \ \#\ c'. $$

証明は**オブジェクト側のコスト導出への帰納**による。各規則に対して、当該ノードへの dispatch に
$K$ ステップを費やし、部分式については帰納法の仮定を用いる。合成の際に必要な各ノード不等式
$$ K + \textstyle\sum_i c_i' \;\le\; a\cdot\bigl(1 + \textstyle\sum_i c_i\bigr)
   \qquad (\text{各 } c_i' \le a\,c_i \text{ の下で}) $$
は、$K \le a = \max K$ から従う（`bound1`, `bound2`）。たとえば `kcons` ノードは
$45 + (c_a' + c_b') \le 73\,(1 + c_a + c_b)$、`call` ノードは $64 + (c_e' + c_p') \le 73\,(1 + c_e
+ c_p)$ として閉じる。定数 $a=73$ が**最大**であることが、すべてのノードでこの不等式を一様に
成立させる鍵である。

### 4.4 抽象定理への投入

得られた定数を Layer A に戻すことで、最適性の定理からオーバヘッド仮定を除去する
（`LevinGrounded`）。Layer A が範囲とするオーバヘッド述語 $\mathrm{LinearOverhead}\ ov$ の
$ov = 73$ での住人はまさに `int-efficient` であり、
$$ \mathtt{overhead\text{-}grounded} : \mathrm{LinearOverhead}\ 73
   \;=\; \mathtt{int\text{-}efficient} $$
が成り立つ（`LevinGrounded.agda`）。したがって、探索の定数は
$$ a_q \;=\; 4\,a\,2^{j} \;=\; 4\cdot 73\cdot 2^{j} $$
となり、$a$ はもはや仮定ではなく**定理**として最適性の主張に組み込まれる。この定数 $73$ は
Rocq 側（`SintCostR.v`, `IntEfficientR.v`）でも同一の値として独立に再証明される（第5節）。

### 4.5 モデリング上の確約とスコープ

$a$ を存在主張から定理へ格上げするにあたり、次の3点を確約した：(i) コストモデルを I 言語の
per-node 単位コスト（$\mathit{time}_p$）に固定したこと、(ii) 対象を1変数 I 自己解釈器
$\mathit{sint}$ に限定したこと、(iii) 各ノードの課金を一様に線形係数として適用したこと。これらは
定数を具体化するために不可欠な選択であり、明示しておく。

なお $a=73$ は、**この1変数 I 自己解釈器**の per-node 単位コストにおける定数であって、多変数
WHILE の万能プログラムのオーバヘッドではない。後者は変数リスト長に比例して増大し、実測では
比 $\approx 2350$（$O(\#\mathrm{vars})$）に達する（`docs/EFFICIENCY.md`）。WHILE から I への
翻訳コストは本形式化の対象外である。本節の貢献は「抽象的だったオーバヘッド定数が、明示した
解釈器とコストモデルのもとで、具体的かつ（二重に）機械検証された定理になった」ことであり、
「73 が万能のオーバヘッドである」ことではない。この限界の詳細は第7節で述べる。

---

*（分量目安：本節は組版で約2〜2.5頁。表・数式含む。第5節の Agda/Rocq 分岐表、第7節の限界と相互
参照。）*
