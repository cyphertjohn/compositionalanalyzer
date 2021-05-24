module Logger = Log

module Make (A : Sigs.Domain) = struct
  
  open A

  let is_sat (solver : Z3.Solver.solver) : bool = 
    match (Z3.Solver.check solver []) with
    | Z3.Solver.SATISFIABLE -> true 
    | _ -> false

  let get_model (solver : Z3.Solver.solver) : Z3.Model.model = 
    match (Z3.Solver.get_model solver) with
    | Some model -> model
    | None -> failwith "No model or unchecked solver"

  (*values you have from A
    type t (The type of the domain element)
    val bot : t
    val sing : Z3.Model.model -> t (Produces a domain element that abstracts the given model)
    val join : t -> t -> t (Takes the join of two domain elements)
    val gamma_hat : Z3.context -> t -> Z3.Expr.exr (Produces a formula representation of a domain element
                                                    Make sure to provide the Z3 context as input!)
    val to_string : t -> string (Produces a string representation of the domain element for logging)
  *)

  let alpha_from_below (ctx : Z3.context) (psi : Z3.Expr.expr) : A.t = 
    let solver = Z3.Solver.mk_simple_solver ctx in
    let ans = ref bot in
    Z3.Solver.add solver [psi]; (*<--- Note the syntax for adding to the Z3 solver*)
    Logger.log_line ~level:`trace ("Init phi:");
    Logger.log_line ~level:`trace (Z3.Expr.to_string psi);
    while (is_sat solver) do
      (* Modify the loop condition and fill in the body. You will need some functions from A given in the above comment
         and the functions get_model and is_sat from above. You will also need to add more things to the Z3 solver.
         See above.*)
      let model = get_model solver in
      let singlet = sing model in
      ans := join !ans singlet;
      Logger.log_line ~level:`trace ("Curr elem:");
      Logger.log_line ~level:`trace (A.to_string !ans);
      Z3.Solver.add solver [Z3.Boolean.mk_not ctx (gamma_hat ctx !ans)]
    done;
    !ans
end

module ReduceProd (A:Sigs.Domain) (B:Sigs.Domain) = struct
  module AAbs = Make(A)
  module BAbs = Make(B)

  type t = A.t * B.t

  let bot = (A.bot, B.bot)

  let sing x = (A.sing x, B.sing x)

  let join x y = (A.join (fst x) (fst y), B.join (snd x) (snd y))

  let gamma_hat ctx x = 
    Z3.Boolean.mk_and ctx [A.gamma_hat ctx (fst x); B.gamma_hat ctx (snd x)]

  let to_string x = 
    let a_str = A.to_string (fst x) in
    let b_str = B.to_string (snd x) in
    "Left Domain element:\n" ^ a_str ^ "\n\n" ^ "Right Domain element:\n" ^ b_str ^ "\n\n"

(*  let reduce (a : A.t) (b : B.t) : (A.t * B.t) =
    let ctx = Z3.mk_context [] in
    let gamma_a = A.gamma_hat ctx a in
    let gamma_b = B.gamma_hat ctx b in
    let conj = Z3.Boolean.mk_and ctx [gamma_a; gamma_b] in
    (AAbs.alpha_from_below ctx conj, BAbs.alpha_from_below ctx conj)*)

end