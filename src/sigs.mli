module Expr :
  sig
    type linterm = Int of int | Times of int * string

    type linexp = Add of linterm list

    val linexp_to_string : linexp -> string

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