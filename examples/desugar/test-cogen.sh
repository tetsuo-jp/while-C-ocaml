#!/bin/sh
# test-cogen.sh - DIFFERENTIAL test for the offline specializer `cogen` and its
# generating-extension emitter `cogen --gen`.  For every program p, static input
# s and dynamic input d, both modes must agree with the interpreter:
#     [cogen(p,s)](d)  ==  [[gen(p)](s)](d)  ==  [p]((s.d))
# the last computed by `rundata` (itself validated against `while` by
# test-oracle.sh).  Covers static control (int_min: S-if unrolled), dynamic while
# (int_loop), dynamic if (int_if), and NESTED dynamic control (int_nest: a D-if
# inside a D-while) — over boundary inputs and a small deterministic fuzz.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
[ -x src/core/cogen ] && [ -x src/core/rundata ] || { echo "build: make -C src/core cogen rundata"; exit 1; }
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0
progs="int_min int_loop int_if int_nest"
# ⌜p⌝ and the generating extension ⌜gen(p)⌝ (built once per program)
for p in $progs; do
  $DW --data-if "examples/desugar/$p.while" > "$tmp/$p.data"
  src/core/cogen --gen "$tmp/$p.data" > "$tmp/$p.gen"
done

oracle () { printf "(%s . %s)" "$2" "$3" > "$tmp/o.val"; src/core/rundata "$tmp/$1.data" "$tmp/o.val"; }

diff1 () { # prog s d : assert cogen-target and gen-target both match the oracle
  orc=$(oracle "$1" "$2" "$3")
  printf "%s" "$2" > "$tmp/s.val"; printf "%s" "$3" > "$tmp/d.val"
  src/core/cogen "$tmp/$1.data" "$tmp/s.val" > "$tmp/tb.data" 2>"$tmp/e" || { echo "FAIL cogen($1,$2): $(cat "$tmp/e")"; fail=1; return; }
  gb=$(src/core/rundata "$tmp/tb.data" "$tmp/d.val")              # [cogen(p,s)](d)
  src/core/rundata "$tmp/$1.gen" "$tmp/s.val" > "$tmp/tg.data"    # [gen(p)](s) = target
  gg=$(src/core/rundata "$tmp/tg.data" "$tmp/d.val")              # [[gen(p)](s)](d)
  if [ "$gb" = "$orc" ] && [ "$gg" = "$orc" ]; then echo "ok   $1 s=$2 d=$3 -> $orc"
  else echo "FAIL $1 s=$2 d=$3: cogen=$gb gen=$gg oracle=$orc"; fail=1; fi
}

# boundary dynamic inputs (one per line so the spaces survive word-splitting)
printf '%s\n' "nil" "('a . 'b)" "('x . ('y . 'z))" "('1 . ('2 . ('3 . nil)))" "(nil . nil)" > "$tmp/dyns.txt"
statics="'swap 'id 'tag nil"
echo "-- differential: [cogen(p,s)](d) == [[gen(p)](s)](d) == [p]((s.d)) --"
for p in $progs; do
  for s in $statics; do
    while IFS= read -r d; do diff1 "$p" "$s" "$d"; done < "$tmp/dyns.txt"
  done
done

# small deterministic fuzz on the dynamic input (random nested lists of atoms)
rand_dyn () { awk -v s="$1" -v q="'" 'BEGIN{srand(s);n=int(rand()*5);out="nil";a[1]=q"p";a[2]=q"q";a[3]="nil";for(i=0;i<n;i++){out="("a[int(rand()*3)+1]" . "out")"}print out}'; }
echo "-- fuzz (deterministic seeds) --"
for p in $progs; do
  for seed in 1 2 3 4 5 6; do diff1 "$p" "'k" "$(rand_dyn "$seed")"; done
done
[ $fail -eq 0 ] && echo "cogen: PASS" || { echo "cogen: FAIL"; exit 1; }
