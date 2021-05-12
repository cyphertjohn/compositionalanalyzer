module Make() = struct
  open PathExp

  let ctx = Z3.mk_context []

  module ST = Map.Make(struct type t = string let compare = compare end)

  let sym_table = ref ST.empty

  let set_vars variables =
    List.iter (fun v -> let vsym = Z3.Symbol.mk_string ctx v in
                        sym_table := ST.add v vsym !sym_table) variables

  (*let curr = ref 10

  let mk_fresh_sym () = 
    let fresh = Z3.Symbol.mk_int ctx !curr in
    curr := !curr + 1;
    fresh*)

  type t = { transform : Z3.Expr.expr ST.t;
             guard : Z3.Expr.expr }

  let zero = {transform = ST.empty; guard = Z3.Boolean.mk_false ctx}
  let one = {transform = ST.empty; guard = Z3.Boolean.mk_true ctx}

  let plus left right = 
    let left_eq = ref [] in
    let right_eq = ref [] in
    let transform = 
      let merge v x y = 
        match x, y with
        | Some s, Some t when Z3.Expr.equal s t -> Some s
        | _, _ ->
          let phi = Z3.Arithmetic.Integer.mk_const_s ctx ("phi_" ^ v) in
          let left_term = 
            match x with 
            | Some s -> s
            | None -> Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table) in
          let right_term = 
            match y with 
            | Some t -> t
            | None -> Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table) in
          left_eq := (Z3.Boolean.mk_eq ctx left_term phi) :: (!left_eq);
          right_eq := (Z3.Boolean.mk_eq ctx right_term phi) :: (!right_eq);
          Some phi
      in
      ST.merge merge left.transform right.transform
    in
    let guard = Z3.Boolean.mk_or ctx [(Z3.Boolean.mk_and ctx (left.guard :: (!left_eq))); (Z3.Boolean.mk_and ctx (right.guard :: (!right_eq)))] in
    {transform; guard}

  let mul x y = 
    let left_subst = 
      let sub_pairs = ref [] in
      ST.iter (fun v term -> sub_pairs := (Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table), term) :: !sub_pairs) x.transform;
      List.split !sub_pairs
    in
    let guard = Z3.Boolean.mk_and ctx [x.guard; Z3.Expr.substitute y.guard (fst left_subst) (snd left_subst)] in
    let merge v left right = 
      let new_exp = Z3.Expr.substitute right [Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table)] [left] in
      Some new_exp
    in
    let transform = ST.union merge x.transform y.transform in
    {transform; guard}

  let star (x : t) = x (*TODO*)

  let rec interp_a a = 
    match a with 
    | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
    | Add l -> Z3.Arithmetic.mk_add ctx (List.map interp_a l)
    | Times (n, v) -> Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx n; Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table)]

  let rec interp_c c = 
    match c with 
    | True -> Z3.Boolean.mk_true ctx 
    | False -> Z3.Boolean.mk_false ctx
    | And (a, b) -> Z3.Boolean.mk_and ctx [interp_c a; interp_c b]
    | Or (a, b) -> Z3.Boolean.mk_or ctx [interp_c a; interp_c b]
    | Not a -> Z3.Boolean.mk_not ctx (interp_c a)
    | LessEq(x, y) -> Z3.Arithmetic.mk_le ctx (interp_a x) (interp_a y)
    | Less (x, y) -> Z3.Arithmetic.mk_lt ctx (interp_a x) (interp_a y)
    | GreaterEq (x, y) -> Z3.Arithmetic.mk_ge ctx (interp_a x) (interp_a y)
    | Greater (x, y) -> Z3.Arithmetic.mk_gt ctx (interp_a x) (interp_a y)
    | Equal (x, y) -> Z3.Boolean.mk_eq ctx (interp_a x) (interp_a y)

  let interp s = 
    match s with 
    | Assign (xp, lin) -> 
      let transform_term = interp_a lin in
      let transform = ST.add xp transform_term ST.empty in
      let guard = Z3.Boolean.mk_true ctx in
      {guard; transform}
    | Cond c -> 
      let guard = interp_c c in
      let transform = ST.empty in
      {guard; transform}

  let to_string tr = 
    let transform = tr.transform in
    let transform_list = ref [] in
    ST.iter (fun v term -> 
      transform_list := (v ^ "' := " ^ (Z3.Expr.to_string (Z3.Expr.simplify term None)))::!transform_list) transform;
    let transform_str = String.concat "\n" !transform_list in
    let guard_str = Z3.Expr.to_string (Z3.Expr.simplify tr.guard None) in
    transform_str ^ "\nwhen " ^ guard_str

end