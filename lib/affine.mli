(**Module for affine equality domain*)

(**This functor requires a implementation of rational numbers and produces an implementation of the affine equality domain.*)
module Make :
  functor (A : Sigs.Rational) ->
    sig
      (**A domain element is either top, bottom, or a set of affine equalities. Each set of equalities, [I(m, b, vars)], represents the following: 
      - [m] and [b] are arrays of the same length. For each [i], [m.(i)] is a map from variables to coeficients, and [b.(i)] is the rhs of the equality. 
      Thus suppose [m.(i)] consists of the mapping [x] -> [2] and [y] -> [1] with [b.(i) = 2], then the i'th equality is {i 2x + 1y = 2}.
      - [vars] is a list of variables captured by the domain element.
      
      *)
      type t = Bot | Top | I of A.q Map.Make(String).t array * A.q array * string list

      (**Converts a domain element to a string*)
      val to_string : t -> string

      (**Projects out variables provided as input.*)
      val project : t -> string list -> t

      (**The bottom element of the domain.*)
      val bot : t

      (**The join of two elements.*)
      val join : t -> t -> t

      (**The meet of two elements.*)
      val meet : t -> t -> t

      (**[add_eqs x eqs] interprets [eqs] as a set of affine equalities and meets with [x].*)
      val add_eqs : t -> Sigs.Expr.lineq list -> t

      (**Computes a set of affine equalities consisting of the set of assignments from the Z3 model.*)
      val sing : Z3.Model.model -> t

      (**Converts a domain element to a formula.*)
      val gamma_hat : Z3.context -> t -> Z3.Expr.expr
    end