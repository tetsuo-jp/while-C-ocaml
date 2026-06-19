# なぜ fp3 が rwhile-C-ocaml では速いのか — 隣の rwhile 側からの調査メモ

2026-06-19、rwhile-C-ocaml（横山研）側の調査より。while-C の fp3（cogen）が multi-hour なのに
rwhile では fp2/fp3 が数十秒で済む理由を、両者を実測して切り分けました。

## まず訂正（最初の直感は誤りでした）

当初は「rwhile は残余をネイティブ実行、while-C は万能プログラム `u`（自己解釈器）経由だから遅い」と
考えました。**これは誤りです**：while-C には既に `src/core/RunData.ml`（`rundata`＝プログラム・アズ・データを
直接実行するネイティブ解釈器、rwhile の `d2p` 相当）があり、fp3 検証もそれを使う。よって
**残余の実行方法は主因ではありません**。`test-futamura.sh` の fp1/fp2 デモが `u.core` 経由なのは別問題。

## 実測

rwhile 側（`EvalRwhile.evalProgram` で spec_av を直接実行、p2d ノード数は `count_nodes`）：

| 量 | rwhile-C-ocaml |
|---|---|
| spec_av (p2d) | 817,701 ノード |
| ri_min (p2d) | 163 ノード |
| fp2 `comp2 = [spec_av]((spec_av.ri_min))` | 812,515 ノード |
| **fp3 `comp3 = [spec_av]((spec_av.spec_av))`** | **1,630,121 ノード** |
| comp2+comp3 の構築 | **18.3 s、peak RSS 99 MB** |

while-C 側（`docs/FUTAMURA.md` より）：fp2 `comp2` ~42 min、fp3 `cogen` 29.6 MB・**~2 h・peak RSS 1.68 GB**。

## 結論：差は「出力サイズ」でも「残余実行法」でもなく、**特殊化器を回す速度**

- **出力は同オーダー**：rwhile comp3 ≈ 1.63M ノード、while-C cogen 29.6 MB（ノード換算で概ね同程度）。
- なのに構築は **時間 ~400×（18s vs 2h）・メモリ ~17×（99MB vs 1.68GB）**。
- つまりボトルネックは **コア OCaml 解釈器で specializer（mix / spec_av）を実行して cogen を作る過程**。
  残余のサイズでも、残余の実行経路（rundata はある）でもない。

## 同じ点（＝単純な構造差ではない）

- 両者ともコア解釈器のストアは `type store = (id * valT) list`（連想リスト、`update` は O(n) で前半を作り直す）。
  **rwhile も連想リストのまま 18s/99MB** なので、ストアのデータ構造そのものは主因ではない。
- fp3 到達手法も共通：束縛時刻入力（MKAV）、深さ一般の読み、ストアサイズ `FpN=256`。

## 主因の候補（要プロファイル、仮説）

出力が同程度なのに時間×メモリが桁違い ＝ **同じ結果を出すのに while-C は遥かに多く計算/確保している**。
peak 1.68 GB（rwhile は 99 MB）が強い手掛かりで、最有力は **アロケーション/GC 量**。具体的には：

1. **中間値の no-op 膨張**：`FUTAMURA.md` 自身が指摘する no-op bloat（cogen 29.6→16.3 MB の余地）。
   膨らんだ中間プログラムを、連想リストストアが代入ごとに O(n) コピーする ＝ サイズ×回数で効く。
2. **自己適用時の per-step アロケーション総量 / GC 時間**（1.68 GB RSS の出所）。
3. **mix と spec_av の特殊化アルゴリズム差**（パス数・冗長処理の量）。

## 提案（小さく検証可能）

- 42 分の fp2 構築（または cogen 構築）を `perf` か区間計測で、**(a) GC 時間、(b) 総アロケーション量、
  (c) 自己適用中の中間プログラムのノード数推移、(d) ストア要素数** を測る。1.68 GB から **(a)/(b)** が最有力。
- **rwhile をベースラインに**：同じ「specializer を自分自身に適用して cogen を作る」処理が 18.3 s・99 MB・
  comp3 1.63M ノード。比較対象に使ってください（手法は共通なので公平な比較になります）。
- 仮に no-op bloat が主因なら、**構築中の中間プログラムを途中で簡約（`Simp` 相当）** して肥大を抑えるのが
  効く可能性。ストア構造の入れ替え（Hashtbl 化）は rwhile が連想リストのまま速いことから優先度低。

— 詳細は rwhile-C-ocaml の `FINDINGS_reversible_projections.md`（ゴミ最小化・N-sizing）と
`AGDA_CORRESPONDENCE.md`。質問があれば rwhile 側へ。
