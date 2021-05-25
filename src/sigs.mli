(**This module holds the types and signatures used in the repository.*)

(**Module for holding onto expression data types.*)
module Expr :
  sig
    (**A linear term is either an integer or an integer times a variable.*)
    type linterm = Int of int | Times of int * string

    (**A linear expression is a sum of linear terms.*)
    type linexp = Add of linterm list

    (**Convert to a string.*)
    val linexp_to_string : linexp -> string

    (**A linear equation is made up of two linear expressions*)
    type lineq = Equal of linexp * linexp

    (**An arithmetic expression for this project can be a linear expression or have a mod between two linear expressions. No nested mods or complex expressions to keep things simple.*)
    type arith_expr = Sum of linexp | Mod of linexp * linexp
    
    (**A predicate is either <, <=, >, >=, or == between two arithmetic expressions. *)
    type pred = 
      | LessEq of arith_expr * arith_expr
      | Less of arith_expr * arith_expr
      | GreaterEq of arith_expr * arith_expr
      | Greater of arith_expr * arith_expr
      | Eq of arith_expr * arith_expr

    (**Boolean expressions are boolean combinations of boolean expressions.*)
    type boolexp =
      True
      | False
      | And of boolexp * boolexp
      | Or of boolexp * boolexp
      | Not of boolexp
      | Pred of pred
end 

module Recurrence :
  sig

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

    val recs_to_string : lin_recs -> string

    type loop_counter = K

    type additive_term_sol = 
      | Times of int * loop_counter

    type lin_rec_sol =
      | RecSol of rec_term * additive_term_sol
    
    type lin_recs_sol = 
      | EmptySol
      | InfeasibleSol
      | RecsSol of lin_rec_sol list
end

module PathExp :
  sig    
    type statement = Assign of string * Expr.linexp | Cond of Expr.boolexp
    type 'a pathexp =
      | One
      | Zero
      | Letter of 'a
      | Plus of 'a pathexp * 'a pathexp
      | Mul of 'a pathexp * 'a pathexp
      | Star of 'a pathexp

    val pathexp_to_string : statement pathexp -> string
  end

(**A signature for rational arithmetic.*)
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

(**An abstract domain.*)
module type Domain =
  sig
    type t
    val bot : t
    val sing : Z3.Model.model -> t
    val join : t -> t -> t
    val gamma_hat : Z3.context -> t -> Z3.Expr.expr
    val to_string : t -> string
  end