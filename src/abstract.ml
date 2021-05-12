
module Make (A : sig 
              type t
              val bot : t
              val sing : Z3.context -> Z3.Model.model -> t
              val gamma_hat : Z3.context -> t -> Z3.Expr.expr
              val join : t -> t -> t 
              val to_string : t -> string
            end) = struct
  
  open A

  let is_sat solver = 
    match (Z3.Solver.check solver []) with
    | Z3.Solver.SATISFIABLE -> true 
    | _ -> false

  let get_model solver = 
    match (Z3.Solver.get_model solver) with
    | Some model -> model
    | None -> failwith "No model or unchecked solver"

  let alpha_from_below ?context:(context = None) psi = 
    let ctx = 
      match context with
      | None -> Z3.mk_context []
      | Some x -> x
    in
    let solver = Z3.Solver.mk_simple_solver ctx in
    let ans = ref bot in
    Z3.Solver.add solver [psi];
    while (is_sat solver) do
      let model = get_model solver in
      let singlet = sing ctx model in
      ans := join !ans singlet;
      print_endline "Curr ans:";
      print_endline (A.to_string !ans);
      Z3.Solver.add solver [Z3.Boolean.mk_not ctx (gamma_hat ctx !ans)]
    done;
    !ans
end

(*
let vars = ["x"; "y"; "xp"; "yp"];;

module A = Affine.Make(struct let vars = vars end)

module ARA = Make(A)

let ctx = Z3.mk_context [];;

let psistring = 
  "(declare-fun x () Int)
   (declare-fun y () Int)
   (declare-fun xp () Int)
   (declare-fun yp () Int)
   (assert (= (+ xp yp) (+ x y 1)))
  ";;

let psiastv = Z3.SMT.parse_smtlib2_string ctx psistring [] [] [] [];;

let psi = Z3.Boolean.mk_and ctx (Z3.AST.ASTVector.to_expr_list psiastv);;

print_endline (Z3.Expr.to_string psi);;

(*let solver = Z3.Solver.mk_simple_solver ctx;;
Z3.Solver.add solver [psi];;
Z3.Solver.check solver [];;
let (Some model) = Z3.Solver.get_model solver;;
print_endline (Z3.Model.to_string model);;
let Some assign = (Z3.Model.get_const_interp_e model (Z3.Arithmetic.Integer.mk_const_s ctx "x"));;
print_endline (Z3.Expr.to_string assign);;
let Some assign = (Z3.Model.get_const_interp_e model (Z3.Arithmetic.Integer.mk_const_s ctx "y"));;
print_endline (Z3.Expr.to_string assign);;*)


let res = ARA.alpha_from_below ~context:(Some ctx) psi;;

print_endline (A.to_string res)

*)