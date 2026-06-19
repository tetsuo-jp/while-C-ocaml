#!/bin/sh
# test-futamura.sh - verify the partial evaluator mix.while.
#
# Programs are fed as data with a FIRST-CLASS 'if (DesugarWhile --data-if),
# which mix specializes directly (no if->while expansion).  Residuals are
# produced as programs-as-data and run by the book's universal program `u`,
# which doubles as the oracle (no data->program decoder needed).
#
#   fp1 (compile, STATIC guard):
#     comp = [mix]((int_min . ('S . op)))      [comp](d) == [int_min]((op . d))
#   dynamic-if residualization (DYNAMIC guard, BT='D):
#     comp = [mix]((int_min . ('D . nil)))     [comp]((op . d)) == [int_min]((op . d))
#     and `comp` retains a first-class 'if (the spec_av 'cond analogue).
#
# fp2/fp3 (self-application) are exercised separately; see FUTAMURA.md.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
WHILE=src/core/while
[ -x src/core/while.opt ] && WHILE=src/core/while.opt

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/mix.while       > "$tmp/mix.core"
$DW examples/desugar/universal.while > "$tmp/u.core"
$DW --data-if examples/desugar/int_min.while > "$tmp/intmin.data"
INT=$(cat "$tmp/intmin.data")
fail=0

run    () { printf "(%s . %s)" "$1" "$2" > "$tmp/r.val"; $WHILE "$tmp/u.core" "$tmp/r.val"; }
oracle () { run "$INT" "($1 . $2)"; }                 # [int_min]((op . d))

# fp1: bake op statically, run residual on d, compare to oracle
fp1 () { # op  d
  printf "(%s . ('S . %s))" "$INT" "$1" > "$tmp/in.val"
  $WHILE "$tmp/mix.core" "$tmp/in.val" > "$tmp/comp.data"
  got=$(run "$(cat "$tmp/comp.data")" "$2"); orc=$(oracle "$1" "$2")
  if [ "$got" = "$orc" ]; then echo "ok   [mix]((int_min.$1)) on $2 = $got"
  else echo "FAIL op=$1 d=$2 comp=$got oracle=$orc"; fail=1; fi
}

# dynamic-if: run the SAME residual (op dynamic) on (op . d)
dyn () { # op  d   (uses $COMPD)
  got=$(run "$COMPD" "($1 . $2)"); orc=$(oracle "$1" "$2")
  if [ "$got" = "$orc" ]; then echo "ok   [comp_dyn](($1 . $2)) = $got"
  else echo "FAIL ($1 . $2) comp=$got oracle=$orc"; fail=1; fi
}

echo "-- fp1 (static guard: mix bakes op, picks the branch) --"
fp1 "'swap" "('a . 'b)"
fp1 "'swap" "('x . ('y . 'z))"
fp1 "'id"   "('a . 'b)"
fp1 "'id"   "('p . 'q)"

echo "-- dynamic-if residualization (BT='D: guard stays runtime, residual keeps 'if) --"
printf "(%s . ('D . nil))" "$INT" > "$tmp/ind.val"
$WHILE "$tmp/mix.core" "$tmp/ind.val" > "$tmp/compd.data"
COMPD=$(cat "$tmp/compd.data")
case "$COMPD" in *"'if"*) echo "ok   residual retains a first-class 'if";; *) echo "FAIL residual has no 'if"; fail=1;; esac
dyn "'swap" "('a . 'b)"
dyn "'id"   "('p . 'q)"
dyn "'foo"  "'bar"

echo "-- dynamic-while residualization (int_loop: a dynamic loop is kept as a residual while) --"
$DW --data-if examples/desugar/int_loop.while > "$tmp/loop.data"
LOOP=$(cat "$tmp/loop.data")
printf "(%s . ('S . 'tag))" "$LOOP" > "$tmp/lin.val"
$WHILE "$tmp/mix.core" "$tmp/lin.val" > "$tmp/cl.data"
CL=$(cat "$tmp/cl.data")
case "$CL" in *"'while"*) echo "ok   residual retains a 'while";; *) echo "FAIL residual has no 'while"; fail=1;; esac
loopchk () { # D
  got=$(run "$CL" "$1")
  orc=$(run "$LOOP" "('tag . $1)")
  if [ "$got" = "$orc" ]; then echo "ok   [comp_loop]($1) = $got"
  else echo "FAIL D=$1 comp=$got oracle=$orc"; fail=1; fi
}
loopchk "('a . ('b . ('c . nil)))"
loopchk "nil"
loopchk "('x . nil)"

if [ -x src/core/simp ]; then
  echo "-- residual simplifier (simp) preserves semantics & shrinks --"
  printf "(%s . ('S . 'swap))" "$INT" > "$tmp/sw.val"
  $WHILE "$tmp/mix.core" "$tmp/sw.val" > "$tmp/comp.data"
  src/core/simp "$tmp/comp.data" > "$tmp/comp.simp"
  got=$(run "$(cat "$tmp/comp.simp")" "('a . 'b)"); orc=$(oracle "'swap" "('a . 'b)")
  b0=$(wc -c < "$tmp/comp.data"); b1=$(wc -c < "$tmp/comp.simp")
  if [ "$got" = "$orc" ]; then echo "ok   simp(comp_swap) runs correctly ($b0 -> $b1 bytes)"
  else echo "FAIL simp changed semantics: $got vs $orc"; fail=1; fi
fi

[ $fail -eq 0 ] && echo "PASS" || { echo "FAILURES"; exit 1; }
