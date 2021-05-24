open Sigs
module Logger = Log
open Expr

(*The Z3 context to be used for all Z3 manipulations.*)
let ctx = Z3.mk_context []

type form = Z3.Expr.expr

(*The next few functions interpret types from Sigs as transitions.*)
let rec interp_term a = 
  match a with 
  | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
  | Times (n, v) -> Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx n; Z3.Arithmetic.Integer.mk_const_s ctx v]

let interp_exp (Add l) = 
  if List.length l = 0 then Z3.Arithmetic.Integer.mk_numeral_i ctx 0
  else if List.length l = 1 then interp_term (List.hd l)
  else Z3.Arithmetic.mk_add ctx (List.map interp_term l)

  let interp_aexp x = 
    match x with
    | Sum xp -> interp_exp xp
    | Mod (y, z) -> Z3.Arithmetic.Integer.mk_mod ctx (interp_exp y) (interp_exp z)

  let interp_p p = 
    match p with
    | LessEq(x, y) -> Z3.Arithmetic.mk_le ctx (interp_aexp x) (interp_aexp y)
    | Less (x, y) -> Z3.Arithmetic.mk_lt ctx (interp_aexp x) (interp_aexp y)
    | GreaterEq (x, y) -> Z3.Arithmetic.mk_ge ctx (interp_aexp x) (interp_aexp y)
    | Greater (x, y) -> Z3.Arithmetic.mk_gt ctx (interp_aexp x) (interp_aexp y)
    | Eq (x, y) -> Z3.Boolean.mk_eq ctx (interp_aexp x) (interp_aexp y)

  let bool_exp_to_formula c = 
    let rec aux b = 
      match b with 
      | True -> Z3.Boolean.mk_true ctx 
      | False -> Z3.Boolean.mk_false ctx
      | And (a, b) -> Z3.Boolean.mk_and ctx [aux a; aux b]
      | Or (a, b) -> Z3.Boolean.mk_or ctx [aux a; aux b]
      | Not a -> Z3.Boolean.mk_not ctx (aux a)
      | Pred p -> interp_p p
     in
    (aux c, ctx)

let to_string = Z3.Expr.to_string