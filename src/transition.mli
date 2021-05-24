(**Semiring of transition formulas.*)

(**The context used for all calculations in this module.*)
val ctx : Z3.context
(**A transition formula.*)
type transition
(**Set the set of program variables.*)
val set_prog_vars : string list -> unit
(**Access the program variables.*)
val get_prog_vars : unit -> string list
(**Given a program variable, returns the string representation of its primed counterpart.*)
val get_prime : string -> string
(**The zero of the semiring. Essentially false.*)
val zero : transition
(**The one of the semiring. {b xp}={b x}.*)
val one : transition
(**The plus. Essentially logical or.*)
val plus : transition -> transition -> transition
(**Extend two transition formulas.*)
val mul : transition -> transition-> transition
(**Intepret a statement as a transition formula.*)
val interp : Sigs.PathExp.statement -> transition
(**Check whether the boolean expression holds at the post state.*)
val check_assert : transition -> Sigs.Expr.boolexp -> bool
(**Convert to a Z3 formula*)
val to_formula : transition -> Z3.Expr.expr * Z3.context
(**A string representation.*)
val to_string : transition -> string
(**Get the pre state of a transition. That is an identity transition, if the pre-state holds, 0 otherwise.*)
val get_pre : transition -> transition
(**Get the post state of a transition. That is an identity transition, if the post-state holds, 0 otherwise.*)
val get_post : transition -> transition
(**The negation of the pre-state.*)
val neg_pre : transition -> transition
(**Get the program vars in a transition.*)
val get_vars : transition -> string list
(**"Simplifies" the transition formula by projecting out skolem variables and using Z3's quantifier elimination tactic.*)
val simplify : transition -> transition
(**"Simplifies" the transition formula by projecting out skolem variables and using Z3's quantifier elimination light tactic.*)
val simplify_light : transition -> transition
(**Return a program summary of the give path expression. This is only meant for loop free code.*)
val analyze_path_exp : Sigs.PathExp.statement Sigs.PathExp.pathexp -> string list -> transition
(**Return a program summary and a boolean indicating whether the given boolean expression holds at the end of the program. Only meant for loop free code.*)
val analyze_path_exp_assertion : Sigs.PathExp.statement Sigs.PathExp.pathexp -> Sigs.Expr.boolexp -> string list -> transition * bool
