(**A formula.*)
type form = Z3.Expr.expr
(**Convert a boolexp to a formula and return the context.*)
val bool_exp_to_formula : Sigs.Expr.boolexp -> form * Z3.context
(**A string representation.*)
val to_string : form -> string
