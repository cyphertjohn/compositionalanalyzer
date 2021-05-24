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

(*This functor takes in two abstract domains and creates a product domain *)
module Prod (A:Sigs.Domain) (B:Sigs.Domain) = struct

  include Make(struct

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

  end)

  (* You have these functions at your disposal:
    val A.bot : A.t
    val A.sing : Z3.Model.model -> A.t
    val A.join : A.t -> A.t -> A.t
    val A.gamma_hat : Z3.context -> A.t -> Z3.Expr.expr
    val A.to_string : A.t -> string
    val B.bot : B.t
    val B.sing : Z3.Model.model -> B.t
    val B.join : B.t -> B.t -> B.t
    val B.gamma_hat : Z3.context -> B.t -> Z3.Expr.expr
    val B.to_string : B.t -> string
    val bot : A.t * B.t
    val sing : Z3.Model.model -> A.t * B.t
    val join : A.t * B.t -> A.t * B.t -> A.t * B.t
    val gamma_hat : Z3.context -> A.t * B.t -> Z3.Expr.expr
    val to_string : A.t * B.t -> string
    val alpha_from_below : Z3.context -> Z3.Expr.expr -> A.t * B.t
    *)

    let reduce (a : A.t) (b : B.t) : (A.t * B.t) =
      let ctx = Z3.mk_context [] in
      let gamma_a = A.gamma_hat ctx a in
      let gamma_b = B.gamma_hat ctx b in
      let conj = Z3.Boolean.mk_and ctx [gamma_a; gamma_b] in
      alpha_from_below ctx conj
end