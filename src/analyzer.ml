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

module F = Formula

module AffineEqs = Abstract.Make(Affine.Make(Q))

module Parity = Abstract.Make(Parity)

module ParityTimesAffine = Abstract.Prod(Parity)(Affine.Make(Q))

let log_out_file = ref false

let get_form input_file = 
  let ic = open_in input_file in 
  let (boolexp, vars) = Par.main Lex.token (Lexing.from_channel ic) in
  let (form, ctx) = Formula.bool_exp_to_formula boolexp in
  close_in ic;
  form, ctx

let setOut fileName = 
  log_out_file := true;
  Logger.set_chan (open_out fileName)
       
let input_files = ref []
  
let compute_reduce_product = ref false
  
let anon_fun file = input_files := file :: !input_files

let abstract_files _ =
  let parity_form, ctx = get_form (List.nth !input_files 0) in
  let parity_val = Parity.alpha_from_below ctx parity_form in
  Logger.log_line "Parity abstraction:";
  Logger.log_line ((Parity.to_string parity_val) ^ "\n");
  if (List.length !input_files > 1) then 
    let (eq_form, eq_ctx) = get_form (List.nth !input_files 1) in
    let aff_val = AffineEqs.alpha_from_below ctx eq_form in
    Logger.log_line "Affine abstraction:";
    Logger.log_line ((AffineEqs.to_string aff_val) ^ "\n");
    if !compute_reduce_product then
      let prod = ParityTimesAffine.reduce parity_val aff_val in
      Logger.log_line "Reduced Product abstraction:";
      Logger.log_line (ParityTimesAffine.to_string prod ^ "\n")
    else () 
  else ();
  if (!log_out_file) then Logger.close ()
  else ()



let register () = 
  let speclist = [("-o", Arg.String setOut, "Set an output file"); 
                  ("-v", Arg.String Logger.set_level, "Set versbosity [trace | debug | always]");
                  ("-prod", Arg.Set compute_reduce_product, "If given an eq formula compute a reduced product");
                  ("-time", Arg.Set Logger.log_times, "Log execution times")] in
  let usage = "analyzer.native <parity-form> [<eq-form>]" in
  Arg.parse speclist anon_fun usage

let () =
  register ();
  input_files := List.rev !input_files;
  abstract_files ()
;;