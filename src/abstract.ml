module Logger = Log

module Make (A : Sigs.Domain) = struct
  
  include A

  let is_sat solver = 
    match (Z3.Solver.check solver []) with
    | Z3.Solver.SATISFIABLE -> true 
    | _ -> false

  let get_model solver = 
    match (Z3.Solver.get_model solver) with
    | Some model -> model
    | None -> failwith "No model or unchecked solver"

  let alpha_from_below (ctx : Z3.context) (psi : Z3.Expr.expr) : A.t = 
    let solver = Z3.Solver.mk_simple_solver ctx in
    let ans = ref bot in
    Z3.Solver.add solver [psi];
    Logger.log_line ~level:`trace ("Init phi:");
    Logger.log_line ~level:`trace (Z3.Expr.to_string psi);
    while (is_sat solver) do
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

  let reduce (a : A.t) (b : B.t) : (A.t * B.t) =
    let ctx = Z3.mk_context [] in
    let gamma_a = A.gamma_hat ctx a in
    let gamma_b = B.gamma_hat ctx b in
    let conj = Z3.Boolean.mk_and ctx [gamma_a; gamma_b] in
    (AAbs.alpha_from_below ctx conj, BAbs.alpha_from_below ctx conj)

end