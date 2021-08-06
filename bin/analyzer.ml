open Comp

module Logger = Log

module CRA = Cra

module A = Affine.Make(Cra.Q)
module ARA = Abstract.Make(A)

let log_out_file = ref false

let analyze_file in_file_name =
  CRA.analyze_file in_file_name;
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