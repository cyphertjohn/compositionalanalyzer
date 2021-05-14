
module A = Affine.Make()

module ARA = Abstract.Make(A)

module Make (D : sig type t
                 val one : t
                 val zero : t
                 val mul : t -> t -> t 
                 val plus : t -> t -> t
                 val star : t -> t
                 val interp : PathExp.statement -> t 
                 val to_string : t -> string 
                 val set_vars : string list -> unit 
                 val check_assert : t -> PathExp.boolexp -> bool
                 val to_formula : t -> Z3.Expr.expr * Z3.context 
                end) : (sig val analyze : in_channel -> unit end) = struct
  

  let rec eval p = (*Could be memoized*)
    match p with
    | PathExp.Letter a -> D.interp a
    | PathExp.One -> D.one
    | PathExp.Zero -> D.zero
    | PathExp.Plus (a, b) -> D.plus (eval a) (eval b)
    | PathExp.Mul (a, b) -> D.mul (eval a) (eval b)
    | PathExp.Star a -> D.star (eval a)

  let analyze file = 
    let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel file) in
    D.set_vars vars;
    A.set_vars (vars @ (List.map (fun v -> v^"'") vars));
    let summary = eval body in
    print_endline (D.to_string summary);
    (match assertion with
    | None -> ()
    | Some a -> 
      if D.check_assert summary a then
        print_endline("PASSED")
      else
        print_endline("FAILED"));
    let (summary_form, ctx) = D.to_formula summary in
    let affine_eq = ARA.alpha_from_below ~context:(ctx) summary_form in
    print_endline ("Affine Equalities");
    print_endline (A.to_string affine_eq)

end

module TR = Make(Transition.Make())

module CRA = Cra.Make()

let analyze_file in_file_name =
  let ic = open_in in_file_name in 
  let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel ic) in
  CRA.set_vars vars;
  let summary = CRA.analyze_path_exp body vars in
  print_endline (CRA.to_string summary);
  (match assertion with
    | None -> ()
    | Some a -> 
      if CRA.check_assert summary a then
        print_endline("PASSED")
      else
        print_endline("FAILED"));
  let (summary_form, ctx) = CRA.to_formula summary in
  A.set_vars vars;
  let affine_eq = ARA.alpha_from_below ~context:(ctx) summary_form in
  print_endline ("Affine Equalities");
  print_endline (A.to_string affine_eq);
  close_in ic


let register () = 
  let speclist = [] in
  let usage = "analyzer.native <while-file>" in
  Arg.parse speclist analyze_file usage

let () =
  register ();;