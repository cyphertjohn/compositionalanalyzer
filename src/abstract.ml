module Logger = Log

module Make (A : Sigs.IDomain) = struct
  
  open A

  let is_sat solver = 
    match (Z3.Solver.check solver []) with
    | Z3.Solver.SATISFIABLE -> true 
    | _ -> false

  let get_model solver = 
    match (Z3.Solver.get_model solver) with
    | Some model -> model
    | None -> failwith "No model or unchecked solver"

  let alpha_from_below ?context:(ctx = Z3.mk_context []) psi = 
    let solver = Z3.Solver.mk_simple_solver ctx in
    let ans = ref bot in
    Z3.Solver.add solver [psi];
    Logger.log_line ~level:`debug ("Init phi:");
    Logger.log_line ~level:`debug (Z3.Expr.to_string psi);
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
