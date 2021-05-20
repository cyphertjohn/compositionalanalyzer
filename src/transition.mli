(**Semiring of transition formulas.*)

(**The context used for all calculations in this module.*)
val ctx : Z3.context
(**A transition formula.*)
type t
(**Set the set of program variables.*)
val set_prog_vars : string list -> unit
(**Access the program variables.*)
val get_prog_vars : unit -> string list
(**Given a program variable, returns the string representation of its primed counterpart.*)
val get_prime : string -> string
val zero : t
val one : t
val plus : t -> t -> t
val mul : t -> t -> t
val interp : Sigs.PathExp.statement -> t
val check_assert : t -> Sigs.Expr.boolexp -> bool
val to_formula : t -> Z3.Expr.expr * Z3.context
val to_string : t -> string
val get_pre : t -> t
val get_post : t -> t
val neg_pre : t -> t
val rec_sol_to_tr : Sigs.Recurrence.lin_recs_sol -> t
