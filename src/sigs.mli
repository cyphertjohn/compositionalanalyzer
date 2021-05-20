module Expr :
  sig
    type linterm = Int of int | Times of int * string

    type linexp = Add of linterm list

    val linexp_to_string : linexp -> string

    type lineq = Equal of linexp * linexp
    
    type pred = 
      | LessEq of linexp * linexp
      | Less of linexp * linexp
      | GreaterEq of linexp * linexp
      | Greater of linexp * linexp
      | Eq of lineq

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

module type IDomain =
  sig
    type t
    val bot : t
    val sing : Z3.Model.model -> t
    val join : t -> t -> t
    val gamma_hat : Z3.context -> t -> Z3.Expr.expr
    val to_string : t -> string
  end