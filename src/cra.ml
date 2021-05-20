open Sigs

module Logger = Log

module Q = struct
  type q = Mpqf.t
  type z = Mpzf.t
  let add = Mpqf.add
  let mul = Mpqf.mul
  let div = Mpqf.div
  let neg = Mpqf.neg
  let is_zero c = (Mpqf.cmp_int c 0) = 0
  let is_one c = (Mpqf.cmp_int c 1) = 0
  let to_string = Mpqf.to_string
  let from_string s =
    let s = 
      if (s.[0]='(') then
        let trimmed = String.sub s 1 ((String.length s) - 2) in
        String.concat "" (String.split_on_char ' ' trimmed)
      else s
    in
    Mpqf.of_string s
  let cmp = Mpqf.cmp
  let get_den = Mpqf.get_den
  let z_to_q = Mpqf.of_mpz
  let z_of_string = Mpzf.of_string
  let lcm = Mpzf.lcm
end


module Make () = struct

  include Transition

  module A = Affine.Make(Q)

  module ARA = Abstract.Make(A)

  module S = Map.Make(String)


(*  let closure aff pre post = 
    let prog_vars = get_prog_vars () in
    let delta_map = ref S.empty in
    let delta_vars = List.map (fun v -> delta_map := S.add ("d"^v) v !delta_map; "d"^v) prog_vars in
    let mk_delta_eq delta =
      let lhs = Sigs.Expr.Add [Sigs.Expr.Times (1, Z3.Symbol.get_string (get_psymbol (S.find delta !delta_map))); Sigs.Expr.Times (-1, Z3.Symbol.get_string (get_symbol (S.find delta !delta_map)))] in
      let rhs = Sigs.Expr.Add [Sigs.Expr.Times (1,  delta)] in
      Sigs.Expr.Equal (lhs, rhs)
    in
    let delts_eq = List.map mk_delta_eq delta_vars in
    let extra_eqs = A.add_eqs aff delts_eq in
    let delta_only = A.project extra_eqs delta_vars in
    match delta_only with
    | A.Top -> 
      let transform = ref S.empty in
      List.iter (fun v -> transform := S.add v (make_havoc ()) !transform) prog_vars;
      let guard = Z3.Boolean.mk_true ctx in
      {transform = !transform; guard}
    | A.Bot -> (* This case shouldn't be possible *)
      let guard = Z3.Boolean.mk_false ctx in
      {transform = S.empty; guard}
    | A.I(m, b, vars) -> 
      let loop_counter = Z3.Quantifier.mk_bound ctx 0 (Z3.Arithmetic.Integer.mk_sort ctx) in
      let guard = ref (Z3.Boolean.mk_true ctx) in
      let transforms = Array.mapi
        (fun i row -> 
              let increase = Z3.Arithmetic.mk_mul ctx [loop_counter; Z3.Arithmetic.Integer.mk_numeral_s ctx (Q.to_string b.(i))] in
              let transf = ref S.empty in
              let folder dvar coef acc = 
                let variable = S.find dvar !delta_map in
                let fresh = make_fresh variable in
                transf := S.add variable fresh !transf;
                let lhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (Q.to_string coef);fresh] in
                let rhsterm = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_s ctx (Q.to_string coef); Z3.Arithmetic.Integer.mk_const ctx (get_symbol variable)] in
                (lhsterm :: (fst acc), rhsterm :: (snd acc))
              in 
              let (lhsl, rhsl) = S.fold folder row ([], []) in
              let lhs = Z3.Arithmetic.mk_add ctx lhsl in
              let rhs = Z3.Arithmetic.mk_add ctx (increase :: rhsl) in
              guard := Z3.Boolean.mk_and ctx [Z3.Boolean.mk_eq ctx lhs rhs; !guard];
              !transf
            ) m in
      let union = S.union (fun _ _ _ -> failwith "Conflict in merge: Variable Multiple times in transform") in
      let loop_transform = Array.fold_left union S.empty transforms in
      let counter_zero = Z3.Boolean.mk_eq ctx loop_counter (Z3.Arithmetic.Integer.mk_numeral_i ctx 0) in
      let no_iterations = {transform = S.empty; guard = Z3.Boolean.mk_and ctx [counter_zero; Z3.Boolean.mk_not ctx pre.guard]} in
      let counter_ge1 = Z3.Arithmetic.mk_ge ctx loop_counter (Z3.Arithmetic.Integer.mk_numeral_i ctx 1) in
      let some_iterations = {transform = loop_transform; guard = Z3.Expr.simplify (Z3.Boolean.mk_and ctx [counter_ge1;!guard]) None} in
      Logger.log_line ~level:`debug "Pre";
      Logger.log_line ~level:`debug (to_string pre);
      Logger.log_line ~level:`debug "Some iterations";
      Logger.log_line ~level:`debug (to_string some_iterations);
      Logger.log_line ~level:`debug "Post";
      Logger.log_line ~level:`debug (to_string post);
      let loop_sum = mul (mul pre some_iterations) post in
      Logger.log_line ~level:`debug "Loop Sum";
      Logger.log_line ~level:`debug (to_string loop_sum);
      let non_quant = plus no_iterations loop_sum in
      let loop_sym = Z3.Symbol.mk_string ctx "K" in
      let quant_guard = Z3.Quantifier.mk_exists ctx [Z3.Arithmetic.Integer.mk_sort ctx] [loop_sym] non_quant.guard None [] [] None None in
      {transform = non_quant.transform; guard = Z3.Quantifier.expr_of_quantifier quant_guard}
*)

  let extract_recs aff_eq = 
    let prog_vars = get_prog_vars () in
    let delta_map = ref S.empty in
    let delta_vars = List.map (fun v -> delta_map := S.add ("d"^v) v !delta_map; "d"^v) prog_vars in
    let mk_delta_eq delta =
      let lhs = Sigs.Expr.Add [Sigs.Expr.Times (1, Z3.Symbol.get_string (get_psymbol (S.find delta !delta_map))); Sigs.Expr.Times (-1, Z3.Symbol.get_string (get_symbol (S.find delta !delta_map)))] in
      let rhs = Sigs.Expr.Add [Sigs.Expr.Times (1,  delta)] in
      Sigs.Expr.Equal (lhs, rhs)
    in
    let delts_eq = List.map mk_delta_eq delta_vars in
    let extra_eqs = A.add_eqs aff_eq delts_eq in
    let delta_only = A.project extra_eqs delta_vars in
    match delta_only with
    | A.Top -> 
      Sigs.Recurrence.Empty
    | A.Bot ->
      Sigs.Recurrence.Infeasible
    | A.I(m, b, vars) -> 
      let extract_rec i row =
        let folder dvar coef acc =
          if Q.is_zero coef then acc
          else
            let var = S.find dvar !delta_map in
            let coef_i = int_of_string (Q.to_string coef) in
            Sigs.Expr.Times (coef_i, var) :: acc
        in
        Sigs.Recurrence.Rec (Sigs.Recurrence.Term (Sigs.Expr.Add (S.fold folder row [])), Sigs.Recurrence.Inc (int_of_string (Q.to_string b.(i))))
      in
      Sigs.Recurrence.Recs (Array.to_list (Array.mapi extract_rec m))

  let solve_rec (Sigs.Recurrence.Rec (Sigs.Recurrence.Term rec_term, Sigs.Recurrence.Inc inc)) =
    Sigs.Recurrence.RecSol (Sigs.Recurrence.Term rec_term, Sigs.Recurrence.Times (inc, Sigs.Recurrence.K)) 

  let solve_recs recurs = 
    match recurs with
    | Sigs.Recurrence.Empty -> Sigs.Recurrence.EmptySol
    | Sigs.Recurrence.Infeasible -> Sigs.Recurrence.InfeasibleSol
    | Sigs.Recurrence.Recs recurrences -> Sigs.Recurrence.RecsSol (List.map solve_rec recurrences)

  let star tr = 
    let form, _ = to_formula tr in
    let pre = get_pre tr in
    let post = get_post tr in
    let not_pre = neg_pre tr in
    let aff_eq = ARA.alpha_from_below ~context:(ctx) form in
    let recs = extract_recs aff_eq in
    let sols = solve_recs recs in
    let some_iters = rec_sol_to_tr sols in
    plus not_pre (mul (mul pre some_iters) post)
    (*closure aff_eq pre post*)

  let rec eval p = (*Could be memoized*)
    match p with
    | PathExp.Letter a -> interp a
    | PathExp.One -> one
    | PathExp.Zero -> zero
    | PathExp.Plus (a, b) -> plus (eval a) (eval b)
    | PathExp.Mul (a, b) -> mul (eval a) (eval b)
    | PathExp.Star a -> star (eval a)

  let analyze_path_exp path_exp vars = 
    set_prog_vars vars;
    let summary = eval path_exp in
    summary
    

  let analyze_path_exp_assertion path_exp assertion vars = 
    let summary = analyze_path_exp path_exp vars in
    (summary, check_assert summary assertion)

end