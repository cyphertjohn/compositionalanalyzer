module Logger = Log

module Make () = struct

  include Transition.Make() 

  module A = Affine.Make()

  module ARA = Abstract.Make(A)

  let curr = ref 0

  module Sym = Map.Make(struct type t = Z3.Symbol.symbol let compare x y = compare (Z3.Symbol.to_string x) (Z3.Symbol.to_string x) end)

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
    | A.I(m, b, vars) -> 
      let loop_counter = Z3.Quantifier.mk_bound ctx 0 (Z3.Arithmetic.Integer.mk_sort ctx) in
      let guard = ref (Z3.Boolean.mk_true ctx) in
      let transforms = Array.mapi
        (fun i row -> 
          let increase = Z3.Arithmetic.mk_mul ctx [loop_counter; Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c b.(i))] in
          let transf = ref ST.empty in
          let folder dvar coef acc = 
            let variable = ST.find (Z3.Symbol.get_string dvar) delta_map in
            let fresh = make_fresh variable in
            transf := ST.add variable fresh !transf;
            let lhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef);fresh] in
            let rhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string_c coef); Z3.Arithmetic.Integer.mk_const ctx (ST.find variable !sym_table)] in
            (lhsterm :: (fst acc), rhsterm :: (snd acc))
          in 
          let (lhsl, rhsl) = A.S.fold folder row ([], []) in
            let lhs = Z3.Arithmetic.mk_add ctx lhsl in
            let rhs = Z3.Arithmetic.mk_add ctx (increase :: rhsl) in
            guard := Z3.Boolean.mk_and ctx [Z3.Boolean.mk_eq ctx lhs rhs; !guard];
            !transf
        ) m in
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
    let delta_sym = ref [] in
    let delta_eqs = List.map 
      (fun v -> 
        let delta_s = Z3.Symbol.mk_string ctx ("d"^v) in
        delta_sym := delta_s :: !delta_sym;
        delta_map := ST.add ("d"^v) v !delta_map;
        let delta_const = Z3.Arithmetic.Integer.mk_const ctx delta_s in
        let rhs = Z3.Arithmetic.mk_sub ctx [Z3.Arithmetic.Integer.mk_const ctx (ST.find v !psym_table); Z3.Arithmetic.Integer.mk_const ctx (ST.find v !sym_table)] in
        Z3.Boolean.mk_eq ctx delta_const rhs
        ) !vars
    in
    let new_form = Z3.Boolean.mk_and ctx (form :: delta_eqs) in
    let primed_and_unprimed = ref [] in
    ST.iter (fun _ sym -> primed_and_unprimed := sym :: !primed_and_unprimed) !sym_table;
    ST.iter (fun _ sym -> primed_and_unprimed := sym :: !primed_and_unprimed) !psym_table;
    let projected_form = project_vars new_form !primed_and_unprimed in
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
    let summary = eval path_exp in
    summary
    

  let analyze_path_exp_assertion path_exp assertion vars = 
    let summary = analyze_path_exp path_exp vars in
    (summary, check_assert summary assertion)

end