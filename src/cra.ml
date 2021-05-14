

module Make () = struct

  include Transition.Make() 

  module A = Affine.Make()

  module ARA = Abstract.Make(A)

  let project_vars form var_symbols = 
    let bound_vars = List.mapi (fun i _ -> Z3.Quantifier.mk_bound ctx i (Z3.Arithmetic.Integer.mk_sort ctx)) var_symbols in
    let subst_form = Z3.Expr.substitute form (List.map (Z3.Arithmetic.Integer.mk_const ctx) var_symbols) bound_vars in
    let quant = Z3.Quantifier.mk_exists ctx (List.map (fun _ -> Z3.Arithmetic.Integer.mk_sort ctx) bound_vars) var_symbols subst_form None [] [] None None in
    Z3.Quantifier.expr_of_quantifier quant

  let curr = ref 0

  let make_havoc () =
    let sym = Z3.Symbol.mk_string ctx ("havoc_" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym

  let make_fresh v = 
    let sym = Z3.Symbol.mk_string ctx (v ^"!" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym

  let closure aff delta_map = 
    match aff with
    | A.Top -> 
      let transform = ref ST.empty in
      List.iter (fun v -> transform := ST.add v (make_havoc ()) !transform) !vars;
      let guard = Z3.Boolean.mk_true ctx in
      {transform = !transform; guard}
    | A.Bot -> (* This case shouldn't be possible *)
      let transform = ref ST.empty in
      let guard = Z3.Boolean.mk_false ctx in
      {transform = !transform; guard}
    | A.I(a, b) -> 
      let loop_counter = Z3.Quantifier.mk_bound ctx 0 (Z3.Arithmetic.Integer.mk_sort ctx) in
      let guard = ref (Z3.Boolean.mk_true ctx) in
      let transforms = Array.mapi
        (fun i row -> 
          let increase = Z3.Arithmetic.mk_mul ctx [loop_counter; Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c b.(i))] in
          let non_zero_entries = List.filter (fun (_, x) -> not (A.is_zero x)) (List.mapi (fun i y -> (i, y)) (Array.to_list row)) in
          (*if List.length non_zero_entries = 1 then
            (let (vari, coef) = List.hd non_zero_entries in
            let variable = ST.find (List.nth !A.vars vari) delta_map
            if A.is_one coef then
              let term = Z3.Arithmetic.mk_add ctx [Z3.Arithmetic.Integer.mk_const ctx (ST.find variable !sym_table); increase] in
              ST.add variable term ST.empty
            else
              let fresh = make_fresh () in
              let lhs = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef); fresh] in
              let rhs = Z3.Arithmetic.mk_add ctx [Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef); Z3.Arithmetic.Integer.mk_const ctx (ST.find variable !sym_table)]; increase] in
              guard := Z3.Boolean.mk_and ctx [Z3.Boolean.mk_eq ctx lhs rhs; !guard];
              ST.add variable fresh ST.empty
              )
          else*)
            let transf = ref ST.empty in
            let (lhsterms, rhsterms) = List.split (List.map 
              (fun (vari, coef) -> 
                let variable = ST.find (List.nth !A.vars vari) delta_map in
                let fresh = make_fresh variable in
                let lhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef); fresh] in
                let rhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef); Z3.Arithmetic.Integer.mk_const ctx (ST.find variable !sym_table)] in
                transf := ST.add variable fresh !transf;
                (lhsterm, rhsterm)
              ) non_zero_entries) in
            let lhs = Z3.Arithmetic.mk_add ctx lhsterms in
            let rhs = Z3.Arithmetic.mk_add ctx (increase :: rhsterms) in
            guard := Z3.Boolean.mk_and ctx [Z3.Boolean.mk_eq ctx lhs rhs; !guard];
            !transf
        ) a in
      let union = ST.union (fun _ _ _ -> failwith "Conflict in merge: Variable Multiple times in transform") in
      let loop_transform = Array.fold_left union ST.empty transforms in
      let counter_zero = Z3.Boolean.mk_eq ctx loop_counter (Z3.Arithmetic.Integer.mk_numeral_i ctx 0) in
      let no_iterations = {transform = ST.empty; guard = counter_zero} in
      let counter_ge1 = Z3.Arithmetic.mk_ge ctx loop_counter (Z3.Arithmetic.Integer.mk_numeral_i ctx 1) in
      let some_iterations = {transform = loop_transform; guard = Z3.Expr.simplify (Z3.Boolean.mk_and ctx [counter_ge1;!guard]) None} in
      let non_quant = plus no_iterations some_iterations in
      let loop_sym = Z3.Symbol.mk_string ctx "K" in
      let quant_guard = Z3.Quantifier.mk_exists ctx [Z3.Arithmetic.Integer.mk_sort ctx] [loop_sym] non_quant.guard None [] [] None None in
      {transform = non_quant.transform; guard = Z3.Quantifier.expr_of_quantifier quant_guard}



  let star tr = 
    let form, _ = to_formula tr in
    let delta_map = ref ST.empty in
    let delta_str = ref [] in
    let delta_eqs = List.map 
      (fun v -> 
        let delta_st = "d"^v in
        delta_str := delta_st :: !delta_str;
        delta_map := ST.add delta_st v !delta_map;
        let delta_const = Z3.Arithmetic.Integer.mk_const ctx (Z3.Symbol.mk_string ctx delta_st) in
        let rhs = Z3.Arithmetic.mk_sub ctx [Z3.Arithmetic.Integer.mk_const ctx (ST.find v !psym_table); Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table)] in
        Z3.Boolean.mk_eq ctx delta_const rhs
        ) !vars
    in
    let new_form = Z3.Boolean.mk_and ctx (form :: delta_eqs) in
    let primed_and_unprimed = ref [] in
    ST.iter (fun _ sym -> primed_and_unprimed := sym :: !primed_and_unprimed) !sym_table;
    ST.iter (fun _ sym -> primed_and_unprimed := sym :: !primed_and_unprimed) !psym_table;
    let projected_form = project_vars new_form !primed_and_unprimed in
    A.set_vars !delta_str;
    let abstracted = ARA.alpha_from_below ~context:(ctx) projected_form in
    closure abstracted !delta_map

  let rec eval p = (*Could be memoized*)
    match p with
    | PathExp.Letter a -> interp a
    | PathExp.One -> one
    | PathExp.Zero -> zero
    | PathExp.Plus (a, b) -> plus (eval a) (eval b)
    | PathExp.Mul (a, b) -> mul (eval a) (eval b)
    | PathExp.Star a -> star (eval a)

  let analyze_path_exp path_exp vars = 
    set_vars vars;
    A.set_vars (vars @ (List.map (fun v -> v^"'") vars));
    let summary = eval path_exp in
    summary
    

  let analyze_path_exp_assertion path_exp assertion vars = 
    let summary = analyze_path_exp path_exp vars in
    (summary, check_assert summary assertion)

end