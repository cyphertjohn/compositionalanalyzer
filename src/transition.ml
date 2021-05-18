module Make() = struct
  open PathExp

  let ctx = Z3.mk_context []

  module ST = Map.Make(struct type t = string let compare = compare end)

  let sym_table = ref ST.empty
  let psym_table = ref ST.empty
  let non_prog_vars = ref []

  let vars = ref []

  let set_vars variables =
    vars := variables;
    List.iter (fun v -> let vsym = Z3.Symbol.mk_string ctx v in
                        sym_table := ST.add v vsym !sym_table;
                        let vpsym = Z3.Symbol.mk_string ctx (v^"'") in
                        psym_table := ST.add v vpsym !psym_table) variables

  let curr = ref 10

  let mk_phi v = 
    let fresh = Z3.Symbol.mk_string ctx ("phi_" ^ v ^ (string_of_int !curr)) in
    curr := !curr + 1;
    non_prog_vars := fresh :: !non_prog_vars;
    fresh

  let make_havoc () =
    let sym = Z3.Symbol.mk_string ctx ("havoc_" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    non_prog_vars := sym :: !non_prog_vars;
    Z3.Arithmetic.Integer.mk_const ctx sym
  
  let make_fresh v = 
    let sym = Z3.Symbol.mk_string ctx (v ^"!" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    non_prog_vars := sym :: !non_prog_vars;
    Z3.Arithmetic.Integer.mk_const ctx sym

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
          let phi_s = mk_phi v in
          let phi = Z3.Arithmetic.Integer.mk_const ctx phi_s in
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

  let check_assert summary asser = 
    let assertion = interp_c asser in
    let transform = summary.transform in
    let guard = summary.guard in
    let subst = 
      let sub_pairs = ref [] in
      ST.iter (fun v term -> sub_pairs := (Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table), term) :: !sub_pairs) transform;
      List.split !sub_pairs
    in
    let sub_assert = Z3.Expr.substitute assertion (fst subst) (snd subst) in
    let final_check = Z3.Boolean.mk_and ctx [guard; Z3.Boolean.mk_not ctx sub_assert] in
    let solver = Z3.Solver.mk_simple_solver ctx in
    Z3.Solver.add solver [final_check];
    match (Z3.Solver.check solver []) with
    | Z3.Solver.UNSATISFIABLE -> true
    | _ -> false

  let project_vars form var_symbols = 
    let bound_vars = List.mapi (fun i _ -> Z3.Quantifier.mk_bound ctx i (Z3.Arithmetic.Integer.mk_sort ctx)) var_symbols in
    let subst_form = Z3.Expr.substitute form (List.map (Z3.Arithmetic.Integer.mk_const ctx) var_symbols) bound_vars in
    let quant = Z3.Quantifier.mk_exists ctx (List.map (fun _ -> Z3.Arithmetic.Integer.mk_sort ctx) bound_vars) var_symbols subst_form None [] [] None None in
    Z3.Expr.simplify (Z3.Quantifier.expr_of_quantifier quant) None

  let to_formula tr =
    let transform = tr.transform in
    let guard = tr.guard in
    let transforms = List.map
      (fun v -> 
        let vp = Z3.Arithmetic.Integer.mk_const ctx (ST.find v !psym_table) in
        try 
          let term = ST.find v transform in
          Z3.Boolean.mk_eq ctx vp term
        with Not_found -> 
          Z3.Boolean.mk_eq ctx vp (Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table))
        ) !vars in
    (project_vars (Z3.Boolean.mk_and ctx (guard :: transforms)) !non_prog_vars, ctx)

  let to_string tr = 
    let transform = tr.transform in
    let transform_list = ref [] in
    ST.iter (fun v term -> 
      transform_list := (v ^ "' := " ^ (Z3.Expr.to_string (Z3.Expr.simplify term None)))::!transform_list) transform;
    let transform_str = String.concat "\n" !transform_list in
    let guard_str = Z3.Expr.to_string (Z3.Expr.simplify tr.guard None) in
    transform_str ^ "\nwhen " ^ guard_str
    (*let (form, _) = to_formula tr in
    Z3.Expr.to_string (Z3.Expr.simplify form None)*)

end