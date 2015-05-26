let parse (c : in_channel) : AbsRwhile.program =
    ParRwhile.pProgram LexRwhile.token (Lexing.from_channel c)

let parseValT (c : in_channel) : AbsRwhile.valT =
    ParRwhile.pValT LexRwhile.token (Lexing.from_channel c)

let showTree (t : AbsRwhile.program) : string =
    PrintRwhile.printTree PrintRwhile.prtProgram t

let showValT (t : AbsRwhile.valT) : string =
    PrintRwhile.printTree PrintRwhile.prtValT t

let () =
  let files = ref [] in
  let f_inv = ref false in
  let f_p2d = ref false in
  let f_exp = ref false in
  Arg.parse
    [("-inverse", Arg.Set f_inv, "inversion");
     ("-p2d", Arg.Set f_p2d, "translation from programs to data");
     ("-exp", Arg.Set f_exp, "expand macro")]
    (fun s -> files := !files @ [s])
    ("R-WHILE Interpreter (C) Tetsuo Yokoyama\n" ^
       Printf.sprintf "usage: %s [-inverse] [-p2d] program [data]" Sys.argv.(0));
  match !files with
  | [prog_filename] ->
     let channel = open_in prog_filename in
     let prog1 = parse channel in
     let _ = close_in channel in
     let prog2 = if !f_exp then MacroRwhile.expMacProgram prog1 else prog1 in
     let prog3 = if !f_inv then InvRwhile.invProgram prog2 else prog2 in
     print_endline (if !f_p2d
		   then showValT (Program2DataRwhile.program2data prog3)
		   else showTree prog3)
  | [prog_filename; data_filename] -> 
     let channel = open_in prog_filename in
     let prog = parse channel in 
     let _ = close_in channel in
     let channel = open_in data_filename in
     let data = parseValT channel in 
     let _ = close_in channel in
     print_endline (showValT (EvalRwhile.evalProgram prog data))
  | _ -> failwith "Invalid arguments"
