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
(**The zero of the semiring. Essentially false.*)
val zero : t
(**The one of the semiring. {b xp}={b x}.*)
val one : t
(**The plus. Essentially logical or.*)
val plus : t -> t -> t
(**Extend two transition formulas.*)
val mul : t -> t -> t
(**Intepret a statement as a transition formula.*)
val interp : Sigs.PathExp.statement -> t
(**Check whether the boolean expression holds at the post state.*)
val check_assert : t -> Sigs.Expr.boolexp -> bool
(**Convert to a Z3 formula*)
val to_formula : t -> Z3.Expr.expr * Z3.context
(**A string representation.*)
val to_string : t -> string
(**Get the pre state of a transition. That is an identity transition, if the pre-state holds, 0 otherwise.*)
val get_pre : t -> t
(**Get the post state of a transition. That is an identity transition, if the post-state holds, 0 otherwise.*)
val get_post : t -> t
(**The negation of the pre-state.*)
val neg_pre : t -> t
(**Convert a recurrence solution to a transition formula.*)
val rec_sol_to_tr : Sigs.Recurrence.lin_recs_sol -> string list -> t
(**Get the program vars in a transition.*)
val get_vars : t -> string list
