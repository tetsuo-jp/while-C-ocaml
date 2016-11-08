let parse (c : in_channel) : AbsWhile.proc =
    ParWhile.pProc LexWhile.token (Lexing.from_channel c)

let parseValT (c : in_channel) : AbsWhile.valT =
    ParWhile.pValT LexWhile.token (Lexing.from_channel c)

let showTree (t : AbsWhile.proc) : string =
    PrintWhile.printTree PrintWhile.prtProc t

let showValT (t : AbsWhile.valT) : string =
    PrintWhile.printTree PrintWhile.prtValT t

let () =
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := !files @ [s])
    ("WHILE Interpreter (C) Tetsuo Yokoyama\n" ^
       Printf.sprintf "usage: %s program data" Sys.argv.(0));
  match !files with
  | [prog_filename; data_filename] -> 
     let channel = open_in prog_filename in
     let prog = parse channel in 
     let _ = close_in channel in
     let channel = open_in data_filename in
     let data = parseValT channel in 
     let _ = close_in channel in
     print_endline (showValT (EvalWhile.evalProc prog data))
  | _ -> failwith "Invalid arguments"
