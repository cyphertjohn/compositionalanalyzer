(**Module for abstracting a formula with an abstract domain. *)

(**A functor which requires a domain as input, and produces an implementation of alpha from below.*)
module Make :
  functor (A : Sigs.Domain) ->
    (**An implementation of alpha from below. If the domain does not satisfy the ascending chain condition or if the input formula is from an undecidable logic,
      alpha from below may not terminate.*)
    sig
      (**The type of the domain element*)
      type t = A.t
      (**The bottom element*)
      val bot : t
      (**A function that extracts a domain element from a model.*)
      val sing : Z3.Model.model -> t
      (**The join of two elements*)
      val join : t -> t -> t
      (**Converts a domain element to a formula*)
      val gamma_hat : Z3.context -> t -> Z3.Expr.expr
      (**Converts a domain element to a string*)
      val to_string : t -> string
      (**Produces the best element from [A]. Requires the context associated with the input formula.*)
      val alpha_from_below : Z3.context -> Z3.Expr.expr -> A.t
    end

(**A functor that produces a reduced product given two domains.*)
module Prod :
  functor (A : Sigs.Domain) (B : Sigs.Domain) -> 
    sig
      (**The type of the domain element*)
      type t = A.t * B.t
      (**The bottom element*)
      val bot : t
      (**A function that extracts a domain element from a model.*)
      val sing : Z3.Model.model -> t
      (**The join of two elements*)
      val join : t -> t -> t
      (**Converts a domain element to a formula*)
      val gamma_hat : Z3.context -> t -> Z3.Expr.expr
      (**Converts a domain element to a string*)
      val to_string : t -> string
      (**Produces the best element from [A] cross [B]. Requires the context associated with the input formula.*)
      val alpha_from_below : Z3.context -> Z3.Expr.expr -> t
    end