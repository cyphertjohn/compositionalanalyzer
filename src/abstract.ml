module Logger = Log

module Make (A : Sigs.Domain) = struct
  
  include A

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
    let mk_not = Z3.Boolean.mk_not ctx in
    let solver = Z3.Solver.mk_simple_solver ctx in
    let ans = ref bot in
    Z3.Solver.add solver [psi]; (*<--- Note the syntax for adding to the Z3 solver*)
    Logger.log_line ~level:`trace ("Init phi:");
    Logger.log_line ~level:`trace (Z3.Expr.to_string psi);
    while (false) do
      (* TODO: Modify the loop condition and replace the body with your code. You will need some functions from A given in the above comment
         and the functions get_model and is_sat from above. You will also need to add more things to the Z3 solver.
         See above. The loop body should follow fig 1 from http://www.cs.cornell.edu/courses/cs711/2005fa/papers/rsy-vmcai04.pdf 
         pretty closely. Use mk_not form for negation. Feel free to add logging information.*)
      ()
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

  let reduce a b = 
    let ctx = Z3.mk_context [] in
    let mk_and x y = Z3.Boolean.mk_and ctx [x;y] in
    (*TODO: Replace the following line with your code. Hint: You might need the above function, and
      the solution should only take 3-5 lines.*)
    (a, b)
end