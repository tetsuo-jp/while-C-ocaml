#!/bin/sh
# check.sh - type-check every Agda design-spec module under --safe.
# Assumes `agda` is on PATH with the standard-library registered (see README.md).
cd "$(dirname "$0")" || exit 1
fail=0
for m in *.agda; do
  if agda --safe "$m" >/dev/null 2>&1; then echo "ok   $m"
  else echo "FAIL $m"; agda --safe "$m"; fail=1; fi
done
[ $fail -eq 0 ] && echo "agda: PASS" || { echo "agda: FAIL"; exit 1; }
