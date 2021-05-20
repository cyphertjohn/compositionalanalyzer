(**Module for affine equality domain*)


module Make :
  (**This functor requires a implementation of rational numbers and produces an implementation of the affine equality domain.*)
  functor (A : Sigs.Rational) ->
    sig
      (**A domain element is either top, bottom, or an array of affine equalities. Each equality [I(m, b, vars)] *)
      type t = Bot | Top | I of A.q Map.Make(String).t array * A.q array * string list
      val to_string : t -> string
      val project : t -> string list -> t
      val bot : t
      val join : t -> t -> t
      val meet : t -> t -> t
      val add_eqs : t -> Sigs.Expr.lineq list -> t
      val sing : Z3.Model.model -> t
      val gamma_hat : Z3.context -> t -> Z3.Expr.expr
    end