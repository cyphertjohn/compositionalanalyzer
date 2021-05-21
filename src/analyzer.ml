open Sigs

module Logger = Log

module CRA = Cra.Make()

module A = Affine.Make(Cra.Q)
module ARA = Abstract.Make(A)

let log_out_file = ref false

let analyze_file in_file_name =
  let ic = open_in in_file_name in 
  let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel ic) in
  CRA.set_prog_vars vars;
  let summary = CRA.simplify_light (CRA.analyze_path_exp body vars) in
  Logger.log_line (CRA.to_string summary);
  (match assertion with
    | None -> ()
    | Some a -> 
      if CRA.check_assert summary a then
        Logger.log_line "PASSED"
      else
        Logger.log_line "FAILED\n");
  (*let (summary_form, ctx) = CRA.to_formula summary in
  Logger.log_line ~level:`debug (Z3.Expr.to_string summary_form);
  Logger.log_line ~level:`debug "Affine Eqs:";
  Logger.log_line ~level:`debug (A.to_string (ARA.alpha_from_below ctx summary_form));*)
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