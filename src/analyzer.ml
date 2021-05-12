
module Make (D : sig type t
                 val one : t
                 val zero : t
                 val mul : t -> t -> t 
                 val plus : t -> t -> t
                 val star : t -> t
                 val interp : PathExp.statement -> t 
                 val to_string : t -> string 
                 val set_vars : string list -> unit end) : (sig val analyze : in_channel -> unit end) = struct
  
  let rec eval p = (*Could be memoized*)
    match p with
    | PathExp.Letter a -> D.interp a
    | PathExp.One -> D.one
    | PathExp.Zero -> D.zero
    | PathExp.Plus (a, b) -> D.plus (eval a) (eval b)
    | PathExp.Mul (a, b) -> D.mul (eval a) (eval b)
    | PathExp.Star a -> D.star (eval a)

  let analyze file = 
    let (pathexp, vars) = Par.main Lex.token (Lexing.from_channel file) in
    D.set_vars vars;
    let result = eval pathexp in
    print_endline (D.to_string result)

end

module TR = Make(Formula.Make())

let analyze_file in_file_name = 
  let ic = open_in in_file_name in
  TR.analyze ic;
  close_in ic


let register () = 
  let speclist = [] in
  let usage = "analyzer.native <while-file>" in
  Arg.parse speclist analyze_file usage

let () =
  register ();;