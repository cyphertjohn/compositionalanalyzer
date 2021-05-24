module Expr = struct
    type linterm = Int of int | Times of int * string

    let linterm_to_string term = 
      match term with
      | Int n -> string_of_int n
      | Times (n, var) -> 
          (string_of_int n) ^ var

    let linterm_to_string_p term = 
      match term with
      | Int n -> 
        if n < 0 then " - " ^ (string_of_int ((-1) * n))
        else " + " ^ (string_of_int n)
      | Times (n, var) -> 
        if n < 0 then " - " ^ (linterm_to_string (Times ((-1)* n, var)))
        else " + " ^ (linterm_to_string (Times (n, var)))

    type linexp = Add of linterm list

    let linexp_to_string (Add exps) = 
      if List.length exps = 0 then "0"
      else if List.length exps = 1 then linterm_to_string (List.hd exps)
      else
        List.fold_left (fun acc term -> acc ^ (linterm_to_string_p term)) (linterm_to_string (List.hd exps)) (List.tl exps)

    type lineq = Equal of linexp * linexp
    
    type arith_expr = Sum of linexp | Mod of linexp * linexp
    
    type pred = 
      | LessEq of arith_expr * arith_expr
      | Less of arith_expr * arith_expr
      | GreaterEq of arith_expr * arith_expr
      | Greater of arith_expr * arith_expr
      | Eq of arith_expr * arith_expr

    type boolexp =
      True
      | False
      | And of boolexp * boolexp
      | Or of boolexp * boolexp
      | Not of boolexp
      | Pred of pred
end 

module Recurrence =
  struct

    type additive_term = 
      Inc of int
    
    type rec_term = 
      Term of Expr.linexp

    type lin_rec =
      Rec of rec_term * additive_term

    type lin_recs = 
      | Empty
      | Infeasible
      | Recs of lin_rec list

    type loop_counter = K

    type additive_term_sol = 
      | Times of int * loop_counter

    type lin_rec_sol =
      | RecSol of rec_term * additive_term_sol
    
    type lin_recs_sol = 
      | EmptySol
      | InfeasibleSol
      | RecsSol of lin_rec_sol list

    let rec_to_string (Rec (Term recterm, Inc additive)) = 
      let recterm_str = Expr.linexp_to_string recterm in
      ("{" ^ recterm_str ^ "}^[k+1] = " ^ "{" ^ recterm_str ^ "}^[k] " ^ (string_of_int additive))

    let recs_to_string recurs =
      match recurs with
      | Empty -> "[]"
      | Infeasible -> "[Loop body infeasible]"
      | Recs recs -> "[" ^ (String.concat "\n" (List.map rec_to_string recs)) ^ "]"

end

module PathExp =
  struct   
    type statement = Assign of string * Expr.linexp | Cond of Expr.boolexp
    type 'a pathexp =
      | One
      | Zero
      | Letter of 'a
      | Plus of 'a pathexp * 'a pathexp
      | Mul of 'a pathexp * 'a pathexp
      | Star of 'a pathexp
  end

module type Rational = 
  sig
    type q 
    type z
    val add : q -> q -> q
    val mul : q -> q -> q
    val div : q -> q -> q
    val neg : q -> q
    val is_zero : q -> bool
    val is_one : q -> bool
    val to_string : q -> string
    val from_string : string -> q
    val cmp : q -> q -> int
    val get_den : q -> z
    val z_to_q : z -> q
    val lcm : z -> z -> z
    val z_of_string : string -> z
end

module type Domain =
  sig
    type t
    val bot : t
    val sing : Z3.Model.model -> t
    val join : t -> t -> t
    val gamma_hat : Z3.context -> t -> Z3.Expr.expr
    val to_string : t -> string
  end