open Sigs

module Make() = struct
  open PathExp
  open Recurrence
  open Expr


  let ctx = Z3.mk_context []

  module ST = Map.Make(String)

  let sym_table = ref ST.empty
  let psym_table = ref ST.empty

  let vars = ref []

  let set_prog_vars variables =
    vars := variables;
    List.iter (fun v -> let vsym = Z3.Symbol.mk_string ctx v in
                        sym_table := ST.add v vsym !sym_table;
                        let vpsym = Z3.Symbol.mk_string ctx (v^"'") in
                        psym_table := ST.add v vpsym !psym_table) variables

  let get_prog_vars () = !vars

  let curr = ref 10

  let get_symbol v = ST.find v !sym_table

  let get_psymbol v = ST.find v !psym_table

  let mk_phi v = 
    let fresh = Z3.Symbol.mk_string ctx ("phi_" ^ v ^ (string_of_int !curr)) in
    curr := !curr + 1;
    fresh

  let make_havoc () =
    let sym = Z3.Symbol.mk_string ctx ("havoc_" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym
  
    
  let make_fresh v = 
    let sym = Z3.Symbol.mk_string ctx (v ^"!" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym

  let make_loop_counter () =
    let sym = Z3.Symbol.mk_string ctx ("K" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym


  type t = { transform : Z3.Expr.expr ST.t;
             guard : Z3.Expr.expr;
             (*phi_vars : Z3.Expr.expr ST.t;
             skolem_vars : Z3.Expr.expr ST.t*)}

  let zero = {transform = ST.empty; guard = Z3.Boolean.mk_false ctx(*; phi_vars = ST.empty; skolem_vars = ST.empty*)}
  let one = {transform = ST.empty; guard = Z3.Boolean.mk_true ctx(*; phi_vars = ST.empty; skolem_vars = ST.empty*)}

  let rec remove_dups l = 
    match l with
    | [] -> []
    | h :: t -> h::(remove_dups (List.filter (fun x -> x<>h)t))

  let get_vars form = 
    let rec aux e = 
      let e_ast = Z3.Expr.ast_of_expr e in
      if Z3.AST.is_var e_ast then []
      else if Z3.AST.is_quantifier e_ast then aux (Z3.Quantifier.get_body (Z3.Quantifier.quantifier_of_expr e))
      else if Z3.Expr.is_const e then 
        let var = Z3.Expr.to_string e in
        [var]
      else
        let args = Z3.Expr.get_args e in
        if List.length args = 0 then []
        else List.concat (List.map aux args)
    in
    remove_dups (aux form)

  let transform_vars tr = 
    let variables = ST.fold (fun _ term acc -> (get_vars term) @ acc) tr.transform [] in
    remove_dups variables

  let elim_quantifiers_form_light form = 
    let qe_light = Z3.Tactic.mk_tactic ctx "qe-light" in
    let g = Z3.Goal.mk_goal ctx false false false in
    Z3.Goal.add g [form];
    let ar = Z3.Tactic.apply qe_light g None in
    let sgs = Z3.Tactic.ApplyResult.get_subgoals ar in
    Z3.Expr.simplify (Z3.Boolean.mk_and ctx (List.map Z3.Goal.as_expr sgs)) None

  let elim_quantifiers_form form = 
    let qe = Z3.Tactic.mk_tactic ctx "qe" in
    let g = Z3.Goal.mk_goal ctx false false false in
    Z3.Goal.add g [form];
    let ar = Z3.Tactic.apply qe g None in
    let sgs = Z3.Tactic.ApplyResult.get_subgoals ar in
    Z3.Expr.simplify (Z3.Boolean.mk_and ctx (List.map Z3.Goal.as_expr sgs)) None

  let project_vars form variables = 
    let var_symbols = List.map (fun v -> Z3.Symbol.mk_string ctx v) variables in
    let bound_vars = List.mapi (fun i _ -> Z3.Quantifier.mk_bound ctx i (Z3.Arithmetic.Integer.mk_sort ctx)) var_symbols in
    let subst_form = Z3.Expr.substitute form (List.map (Z3.Arithmetic.Integer.mk_const ctx) var_symbols) bound_vars in
    let quant = Z3.Quantifier.mk_exists ctx (List.map (fun _ -> Z3.Arithmetic.Integer.mk_sort ctx) bound_vars) var_symbols subst_form None [] [] None None in
    Z3.Expr.simplify (Z3.Quantifier.expr_of_quantifier quant) None

  let elim_skolem_light tr = 
    let tr_vars = transform_vars tr in
    let guard_vars = get_vars tr.guard in
    let (_, vars_to_remove) = List.partition (fun v -> List.mem v tr_vars || List.mem v !vars) guard_vars in
    let projected_guard = project_vars tr.guard vars_to_remove in
    {transform = tr.transform; guard = elim_quantifiers_form_light projected_guard}


  let elim_skolem tr = 
    let tr_vars = transform_vars tr in
    let guard_vars = get_vars tr.guard in
    let (_, vars_to_remove) = List.partition (fun v -> List.mem v tr_vars || List.mem v !vars) guard_vars in
    let projected_guard = project_vars tr.guard vars_to_remove in
    {transform = tr.transform; guard = elim_quantifiers_form projected_guard}


  let simplify_guard tr = 
    let tr_vars = transform_vars tr in
    let guard_vars = get_vars tr.guard in
    let (_, vars_to_remove) = List.partition (fun v -> List.mem v tr_vars || List.mem v !vars) guard_vars in
    {transform = tr.transform; guard = project_vars tr.guard vars_to_remove}

  let make_fresh_skolems tr = 
    let tr_vars =  transform_vars tr in
    let guard_vars = get_vars tr.guard in
    let skolem_vars = List.filter (fun v -> not (List.mem v !vars)) (remove_dups (tr_vars @ guard_vars)) in
    let fresh_vars = List.map (fun _ -> make_fresh "mid") skolem_vars in
    let subst f = Z3.Expr.substitute f (List.map (Z3.Arithmetic.Integer.mk_const_s ctx) skolem_vars) fresh_vars in
    {transform = ST.map (fun term -> subst term) tr.transform; guard = subst tr.guard}

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

  let mul x y = (*needs to be fixed*)
    let y = make_fresh_skolems y in
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

  let rec interp_term a = 
    match a with 
    | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
    | Times (n, v) -> Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx n; Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table)]

  let interp_exp (Add l) = 
    if List.length l = 0 then Z3.Arithmetic.Integer.mk_numeral_i ctx 0
    else if List.length l = 1 then interp_term (List.hd l)
    else Z3.Arithmetic.mk_add ctx (List.map interp_term l)

  let interp_e (Equal (lhs, rhs)) = Z3.Boolean.mk_eq ctx (interp_exp lhs) (interp_exp rhs)

  let interp_p p = 
    match p with
    | LessEq(x, y) -> Z3.Arithmetic.mk_le ctx (interp_exp x) (interp_exp y)
    | Less (x, y) -> Z3.Arithmetic.mk_lt ctx (interp_exp x) (interp_exp y)
    | GreaterEq (x, y) -> Z3.Arithmetic.mk_ge ctx (interp_exp x) (interp_exp y)
    | Greater (x, y) -> Z3.Arithmetic.mk_gt ctx (interp_exp x) (interp_exp y)
    | Eq e -> interp_e e

  let rec interp_c c = 
    match c with 
    | True -> Z3.Boolean.mk_true ctx 
    | False -> Z3.Boolean.mk_false ctx
    | And (a, b) -> Z3.Boolean.mk_and ctx [interp_c a; interp_c b]
    | Or (a, b) -> Z3.Boolean.mk_or ctx [interp_c a; interp_c b]
    | Not a -> Z3.Boolean.mk_not ctx (interp_c a)
    | Pred p -> interp_p p

  let interp s = 
    match s with 
    | Assign (xp, lin) -> 
      let transform_term = interp_exp lin in
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

  let get_pre tr = 
    {transform = ST.empty; guard = tr.guard}

  let get_post tr =
    let make_fresh_sub (sub_pairs, new_consts) var =
      let fresh = make_fresh var in
      let var_const = Z3.Arithmetic.Integer.mk_const ctx (ST.find var !sym_table) in
      let subst = (var_const, fresh) in
      let new_const = 
        try
          let new_term = Z3.Expr.substitute (ST.find var tr.transform) [var_const] [fresh] in
          [Z3.Boolean.mk_eq ctx var_const new_term]
        with Not_found ->
          []
      in
      (subst :: sub_pairs, new_const @ new_consts)
    in
    let (sub_pairs, new_consts) = List.fold_left make_fresh_sub ([], []) !vars in
    let (var_const, fresh_vars) = List.split sub_pairs in
    let proj_guard = Z3.Expr.substitute tr.guard var_const fresh_vars in
    {transform = ST.empty; guard = Z3.Boolean.mk_and ctx (proj_guard :: new_consts)}


  let neg_pre tr = 
    {transform = ST.empty; guard = Z3.Boolean.mk_not ctx tr.guard}


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
    let form = (Z3.Boolean.mk_and ctx (guard :: transforms)) in
    let form_vars = get_vars form in
    let skolem_vars = List.filter (fun v -> not (List.mem v !vars)) form_vars in
    (project_vars form skolem_vars, ctx)

  let to_string tr = 
    let tr_simp = elim_skolem_light tr in
    let transform = tr_simp.transform in
    let transform_list = ref [] in
    ST.iter (fun v term -> 
      transform_list := (v ^ "' := " ^ (Z3.Expr.to_string (Z3.Expr.simplify term None)))::!transform_list) transform;
    let transform_str = String.concat "\n" !transform_list in
    let guard_str = Z3.Expr.to_string (Z3.Expr.simplify tr_simp.guard None) in
    transform_str ^ "\nwhen " ^ guard_str
    (*let (form, _) = to_formula tr in
    Z3.Expr.to_string (Z3.Expr.simplify form None)*)


end