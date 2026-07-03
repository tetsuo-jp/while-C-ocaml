(** * Extract -- extract the verified interpreter and self-interpreter to OCaml.

    Produces [while_eval.ml]: an executable OCaml interpreter whose behaviour is exactly
    the verified Rocq [eval]/[run]/[sint] (D1). *)
From Stdlib Require Import Extraction.
Require Import UniversalInterp ProgEncoding SelfInterp.
Extraction Language OCaml.
Set Extraction Output Directory ".".
Extraction "while_eval.ml" run sint enc decode.
