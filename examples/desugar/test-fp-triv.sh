#!/bin/sh
# test-fp-triv.sh - the three Futamura projections via the TRIVIAL specializer
# mix_triv.while (Jones's papp closure, structurally residualized).  Unlike the
# optimizing mix.while (fp2 ~22MB/40min, fp3 ~30MB/2h), the trivial specializer
# does O(|P|+|s|) work, so all three projections -- including self-application --
# finish in milliseconds.  The residuals are small and re-run P on (s.d) (no
# optimization); the point is that the projection equations hold and are
# non-vacuous, fast.  Needs: make -C src/core rundata.
set -e
cd "$(dirname "$0")/../.."
DW=src/desugar/DesugarWhile
WHILE=src/core/while
[ -x src/core/while.opt ] && WHILE=src/core/while.opt
RUN=src/core/rundata
[ -x "$RUN" ] || { echo "build src/core/rundata first (make -C src/core rundata)"; exit 1; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
$DW examples/desugar/mix_triv.while > "$tmp/mixt.core"
$DW --data-if examples/desugar/mix_triv.while > "$tmp/mixt.data"
$DW --data-if examples/desugar/int_min.while > "$tmp/int.data"
MT=$(cat "$tmp/mixt.data"); INT=$(cat "$tmp/int.data")
fail=0

oracle () { printf "(%s . %s)" "$1" "$2" > "$tmp/o.val"; $RUN "$tmp/int.data" "$tmp/o.val"; } # [int_min]((op.d))

# fp1: comp = [mix_triv]((int_min . ('S.op)))
printf "(%s . ('S . 'swap))" "$INT" > "$tmp/i1.val"
$WHILE "$tmp/mixt.core" "$tmp/i1.val" > "$tmp/comp.data"
chk () { # label  residual  op  d
  printf "%s" "$3" > "$tmp/op.val"  # unused
  got=$($RUN "$2" "$tmp/$5"); orc=$(oracle "$3" "$4")
  if [ "$got" = "$orc" ]; then echo "ok   $1 = $got"; else echo "FAIL $1: $got vs $orc"; fail=1; fi
}
printf "('a . 'b)" > "$tmp/ab.val"; printf "('p . 'q)" > "$tmp/pq.val"
echo "-- fp1 (trivial compile) --"
chk "[comp_swap](('a.'b))" "$tmp/comp.data" "'swap" "('a . 'b)" ab.val

# fp2: comp2 = [mix_triv]((mix_triv . ('S.int_min)))
echo "-- fp2 (trivial compiler generation) --"
printf "(%s . ('S . %s))" "$MT" "$INT" > "$tmp/i2.val"
$WHILE "$tmp/mixt.core" "$tmp/i2.val" > "$tmp/comp2.data"
echo "   comp2 = $(wc -c < "$tmp/comp2.data") bytes"
printf "('S . 'swap)" > "$tmp/Ssw.val"; printf "('S . 'id)" > "$tmp/Sid.val"
$RUN "$tmp/comp2.data" "$tmp/Ssw.val" > "$tmp/c2sw.data"
cmp -s "$tmp/c2sw.data" "$tmp/comp.data" && echo "ok   [comp2]('S.swap) == fp1 comp" || { echo "FAIL fp2 != fp1"; fail=1; }
chk "[[comp2]('S.swap)](('a.'b))" "$tmp/c2sw.data" "'swap" "('a . 'b)" ab.val

# fp3: cogen = [mix_triv]((mix_triv . ('S.mix_triv)))
echo "-- fp3 (trivial cogen) --"
printf "(%s . ('S . %s))" "$MT" "$MT" > "$tmp/i3.val"
$WHILE "$tmp/mixt.core" "$tmp/i3.val" > "$tmp/cogen.data"
echo "   cogen = $(wc -c < "$tmp/cogen.data") bytes"
printf "('S . %s)" "$INT" > "$tmp/Sint.val"
$RUN "$tmp/cogen.data" "$tmp/Sint.val" > "$tmp/c2b.data"
cmp -s "$tmp/c2b.data" "$tmp/comp2.data" && echo "ok   [cogen]('S.int_min) == comp2" || { echo "FAIL cogen != comp2"; fail=1; }
$RUN "$tmp/c2b.data" "$tmp/Ssw.val" > "$tmp/c2bsw.data"
$RUN "$tmp/c2b.data" "$tmp/Sid.val" > "$tmp/c2bid.data"
chk "[[[cogen]('S.int_min)]('S.swap)](('a.'b))" "$tmp/c2bsw.data" "'swap" "('a . 'b)" ab.val
chk "[[[cogen]('S.int_min)]('S.id)](('p.'q))"   "$tmp/c2bid.data" "'id"   "('p . 'q)" pq.val
cmp -s "$tmp/c2bsw.data" "$tmp/c2bid.data" && { echo "FAIL cogen output ignores op"; fail=1; } || echo "ok   cogen-built compiler uses op (swap != id)"

[ $fail -eq 0 ] && echo "fp-triv: PASS" || { echo "fp-triv: FAILURES"; exit 1; }
