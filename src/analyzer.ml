open Sigs

module Q = struct
  type q = Mpqf.t
  type z = Mpzf.t
  let add = Mpqf.add
  let mul = Mpqf.mul
  let div = Mpqf.div
  let neg = Mpqf.neg
  let is_zero c = (Mpqf.cmp_int c 0) = 0
  let is_one c = (Mpqf.cmp_int c 1) = 0
  let to_string = Mpqf.to_string
  let from_string s =
    let s = 
      if (s.[0]='(') then
        let trimmed = String.sub s 1 ((String.length s) - 2) in
        String.concat "" (String.split_on_char ' ' trimmed)
      else s
    in
    Mpqf.of_string s
  let cmp = Mpqf.cmp
  let get_den = Mpqf.get_den
  let z_to_q = Mpqf.of_mpz
  let z_of_string = Mpzf.of_string
  let lcm = Mpzf.lcm
end

module Logger = Log

module Tr = Transition

module AffineEqs = Abstract.Make(Affine.Make(Q))

module Parity = Abstract.Make(Parity)

module ParityTimesAffine = Abstract.Prod(Parity)(Affine.Make(Q))

let log_out_file = ref false

let analyze_file in_file_name =
  let ic = open_in in_file_name in 
  let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel ic) in
  Tr.set_prog_vars vars;
  let summary = Tr.simplify_light (Tr.analyze_path_exp body vars) in
  (match assertion with
    | None -> ()
    | Some a -> 
      if Tr.check_assert summary a then
        Logger.log_line "Assertion PASSED"
      else
        Logger.log_line "Assertion FAILED\n");
  let summary_form, ctx = Tr.to_formula summary in
  let parity_str = Parity.to_string (Parity.alpha_from_below ctx summary_form) in
  let aff_eq_str = AffineEqs.to_string (AffineEqs.alpha_from_below ctx summary_form) in
  let prod_str = ParityTimesAffine.to_string (ParityTimesAffine.alpha_from_below ctx summary_form) in
  Logger.log_line "Parity abstraction:";
  Logger.log_line (parity_str ^ "\n");

  Logger.log_line "Affine abstraction:";
  Logger.log_line (aff_eq_str ^ "\n");
  
  Logger.log_line "Reduced Product abstraction:";
  Logger.log_line (prod_str ^ "\n");

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