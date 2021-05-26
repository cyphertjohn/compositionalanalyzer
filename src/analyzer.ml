open Sigs

module Logger = Log

module CRA = Cra

module A = Affine.Make(Cra.Q)
module ARA = Abstract.Make(A)

let log_out_file = ref false

let analyze_file in_file_name =
  let ic = open_in in_file_name in 
  let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel ic) in
  CRA.set_prog_vars vars;
  let summary = CRA.simplify_light (CRA.analyze_path_exp body vars) in
  Logger.log_line ~level:`debug "Program summary:";
  Logger.log_line ~level:`debug (CRA.to_string summary);
  Logger.log_line "";
  (match assertion with
    | None -> ()
    | Some a -> 
      if CRA.check_assert summary a then
        Logger.log_line "Assertion PASSED"
      else
        Logger.log_line "Assertion FAILED\n");
  close_in ic;
  if (!log_out_file) then Logger.close ()
  else ()



let setOut fileName = 
  log_out_file := true;
  Logger.set_chan (open_out fileName);;
     
 
let register () = 
  let speclist = [("-o", Arg.String setOut, "Set an output file"); 
                  ("-v", Arg.String Logger.set_level, "Set versbosity [trace | debug | always]");
                  ("-time", Arg.Set Logger.log_times, "Log execution times")] in
  let usage = "analyzer.native <while-file>" in
  Arg.parse speclist analyze_file usage

let () =
  register ();;