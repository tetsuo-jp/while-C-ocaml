#!/bin/sh
# Phase 1 の E2E テスト: timed universal program tt を時間制限の境界で検証する。
# 各プログラム p, 入力 d について、真のステップ数 time_p(d) を --time で求め、
#   n = time-1 なら nil (時間切れ),  n = time なら (⟦p⟧(d) . nil)
# となることを確認する (Definition timed-universal の両分岐)。
#
# 使い方: examples/desugar/test-timed-universal.sh   (リポジトリのルートから)
set -eu

D=src/desugar/DesugarWhile
W=src/core/while
TT=/tmp/tt-test.while
$D examples/desugar/timed-universal.while > "$TT"

# nil^n を作る
mkcount() {
  n=$1; s="nil"
  i=0; while [ "$i" -lt "$n" ]; do s="(nil . $s)"; i=$((i + 1)); done
  echo "$s"
}

fail=0
check() {  # $1=program text  $2=input value  $3=label
  printf '%s' "$1" > /tmp/tu_p.while
  printf '%s' "$2" > /tmp/tu_d.val
  res=$($W /tmp/tu_p.while /tmp/tu_d.val)
  steps=$($W --time /tmp/tu_p.while /tmp/tu_d.val)
  pdata=$($D --data /tmp/tu_p.while)
  PD="($pdata . $2)"

  printf '(%s . %s)' "$PD" "$(mkcount $((steps - 1)))" > /tmp/tu_in.val
  lo=$($W "$TT" /tmp/tu_in.val)
  printf '(%s . %s)' "$PD" "$(mkcount "$steps")" > /tmp/tu_in.val
  ok=$($W "$TT" /tmp/tu_in.val)

  if [ "$lo" = "nil" ] && [ "$ok" = "($res . nil)" ]; then
    echo "ok   $3 (time=$steps)"
  else
    echo "FAIL $3: n<time -> $lo (want nil); n=time -> $ok (want ($res . nil))"
    fail=1
  fi
}

check 'read X; Y := cons nil X; write Y'                          '(nil . nil)'        'succ'
check 'read X; Y := hd X; write Y'                                "('a . 'b)"          'hd'
check 'read X; Y := tl X; write Y'                                "('a . 'b)"          'tl'
check 'read X; Y := =? X X; write Y'                              "'a"                 'eq-true'
check 'read X; Y := nil; while X do { Y := cons (hd X) Y; X := tl X; } write Y' \
                                                                  "('p . ('q . nil))"  'reverse'

[ "$fail" -eq 0 ] && echo "all timed-universal checks passed" || exit 1
