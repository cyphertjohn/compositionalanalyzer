(**Module for abstracting a formula with an abstract domain. *)

(**A functor which requires a domain as input, and produces an implementation of alpha from below.*)
module Make :
  functor (A : Sigs.Domain) ->
    (**An implementation of alpha from below. If the domain does not satisfy the ascending chain condition or if the input formula is from an undecidable logic,
      alpha from below may not terminate.*)
    sig
      (**Produces the best element from [A].*)
      val alpha_from_below : ?context:Z3.context -> Z3.Expr.expr -> A.t
    end
