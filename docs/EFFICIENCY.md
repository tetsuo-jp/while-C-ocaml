# Efficiency of the timed universal program (Phase 2)

This note records the empirical overhead of the timed universal program
`tt` (`examples/desugar/timed-universal.while`), reproducing the content of
ch.19 Lemma "tt is an efficient timed universal program" on concrete runs.

Reproduce with `examples/desugar/bench-efficiency.sh` (from the repo root).
"Steps" are the unit-cost step counts from `while --time` (ch.16-19 model).
We measure

    ratio = time_tt((p.d).nil^n) / time_p(d)   with n = time_p(d),

i.e. the interpretation overhead when `tt` runs `p` on `d` to completion.

## (A) One-variable program — efficient

`read X; while X do { X := tl X; } write X` on inputs of length *len*:

| len | time_p | time_tt | ratio |
|----:|-------:|--------:|------:|
|   1 |      7 |  17 136 | 2 448 |
|   2 |     12 |  28 915 | 2 409 |
|   4 |     22 |  52 473 | 2 385 |
|   8 |     42 |  99 589 | 2 371 |
|  16 |     82 | 193 821 | 2 363 |
|  32 |    162 | 382 285 | 2 359 |

The ratio is **bounded and converges** (≈ 2 350) as the work grows — the
signature of `time_tt = k·time_p + c`, with the fixed start-up cost `c`
amortised away. A program-independent constant `k` is exactly
Definition "efficient-timed-universal". ✔

## (B) k-variable family — the inefficiency

Same loop padded to `k` variables, with the loop body accessing the
`k`-th variable; input length fixed at 16:

| vars | time_p | time_tt | ratio |
|-----:|-------:|--------:|------:|
|    1 |     82 | 193 821 | 2 363 |
|    2 |    132 | 319 646 | 2 421 |
|    3 |    134 | 326 246 | 2 434 |
|    4 |    136 | 332 869 | 2 447 |
|    6 |    140 | 346 184 | 2 472 |
|    8 |    144 | 359 591 | 2 497 |

The ratio **grows monotonically with the number of variables** (≈ +13 per
extra variable here). This is the `O(#vars)` cost of the `lookup`/`update`
loops in `tt`: variable access is not constant-time for multi-variable
programs. It is exactly why the book proves efficiency only for the
**one-variable I language** (and obtains a genuinely efficient self-interpreter
by translating multi-variable WHILE to one variable, Prop. "many-one-var").

## Consequence for the hierarchy theorem

The linear-time hierarchy (Thm. "constant time factors do matter") needs an
*efficient* timed universal program so that `diag` runs within
`a·b·|p|`. Table (A) gives the constant `k` empirically for the
one-variable case; a fully faithful `diag` therefore wants the one-variable
path (Phase 3 / the `many-one-var` translation), not the multi-variable `tt`
measured here. The multi-variable `tt` is still a correct timed universal
program — just not efficient in the technical sense for arbitrary arity.
