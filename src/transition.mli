module Make :
  functor () ->
    sig
      val ctx : Z3.context
      val get_symbol : string -> Z3.Symbol.symbol
      val get_psymbol : string -> Z3.Symbol.symbol
      val get_prog_vars : unit -> string list
      val set_prog_vars : string list -> unit
      val make_havoc : unit -> Z3.Expr.expr
      val make_fresh : string -> Z3.Expr.expr
      val make_loop_counter : unit -> Z3.Expr.expr
      type t = { transform : Z3.Expr.expr Map.Make(String).t; guard : Z3.Expr.expr; }
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
    end
