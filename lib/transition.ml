open Sigs
module Logger = Log

open PathExp
  open Recurrence
  open Expr

  (*The Z3 context to be used for all Z3 manipulations.*)
  let ctx = Z3.mk_context []

  (*String map for various things.*)
  module ST = Map.Make(String)

  (*For each program variable we keep track of a string representation of its prime counterpart.*)
  let prime_table = ref ST.empty

  (*The list of program variables.*)
  let vars = ref []

  let set_prog_vars variables =
    vars := variables;
    List.iter (fun v -> prime_table := ST.add v (v^"p") !prime_table) variables

  let get_prog_vars () = !vars

  (*A counter to generate unique names for skolem variables.*)
  let curr = ref 10

  let get_prime v = ST.find v !prime_table

  (*A map to keep track of the phi variables and their associated program variables. Just here to make the output look nicer.
  A phi variables looks like phi_x10. The map will have phi_x10 -> x.*)
  let phi_vars = ref ST.empty

  (*The same as phi_vars but for other skolem vars and for the same purpose.*)
  let var_skolems = ref ST.empty

  (*Same as previous but we just keep track of a list of loop counter vars.*)
  let loop_counters = ref []

  (*make a fresh phi const*)
  let mk_phi v = 
    let var_string = "phi_" ^ v ^ (string_of_int !curr) in
    phi_vars := ST.add var_string v !phi_vars;
    let fresh = Z3.Symbol.mk_string ctx var_string in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx fresh

  (*make a havoc const*)
  let make_havoc () =
    let sym = Z3.Symbol.mk_string ctx ("havoc_" ^ (string_of_int !curr)) in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym
  
  (*make a fresh skolem var*)
  let make_fresh v = 
    let var_string = v ^"!" ^ (string_of_int !curr) in
    var_skolems := ST.add var_string v !var_skolems;
    let sym = Z3.Symbol.mk_string ctx var_string in
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym

  (*make a fresh loop counter*)
  let make_loop_counter () =
    let var_string = "K!" ^ (string_of_int !curr) in
    let sym = Z3.Symbol.mk_string ctx var_string in
    loop_counters := var_string :: !loop_counters;
    curr := !curr + 1;
    Z3.Arithmetic.Integer.mk_const ctx sym


  (*A transition represents a transition formula as follows:
    - The guard is a formula over program variables and skolem variables. The guard constrains pre-state vars and
      variables used in the transform.
    - The transform represents a map from "prime" vars to a Z3 expression which represents the post-state of that variable.
    The way to read a transition is if the guard holds, then the post state of the vars in the transform is equal to what that
    variable maps to. If a variable is not present in the transform it is implicit that the post-state of that variable is equal
    to the pre-state.
    
    For example the transition
      transform {x -> x + 1, y -> y!52}
      guard: x < 10 /\ z = 0 /\ y!52 + havoc!42 = y + x + 1
      corresponds to the transition formula:
      exists y!52 havoc!42
      x' = x + 1 /\ y' = y!52 /\ z' = z /\ x < 10 /\ z = 0 /\ y!52 + havoc!42 = y + x + 1
    *)
  type transition = { transform : Z3.Expr.expr ST.t;
                      guard : Z3.Expr.expr}

  let zero = {transform = ST.empty; guard = Z3.Boolean.mk_false ctx}
  let one = {transform = ST.empty; guard = Z3.Boolean.mk_true ctx}

  let rec remove_dups l = 
    match l with
    | [] -> []
    | h :: t -> h::(remove_dups (List.filter (fun x -> x<>h)t))

  (*Traverse a Z3 expression for constant names. Warning Z3 will crash if you ask is_const on a quantifier.*)
  let get_vars_form form = 
    let rec aux e = 
      let e_ast = Z3.Expr.ast_of_expr e in
      if Z3.AST.is_var e_ast then []
      else if Z3.AST.is_quantifier e_ast then aux (Z3.Quantifier.get_body (Z3.Quantifier.quantifier_of_expr e))
      else if Z3.Expr.is_const e then 
        if Z3.Boolean.is_true e || Z3.Boolean.is_false e then []
        else
          let var = Z3.Expr.to_string e in
          [var]
      else
        let args = Z3.Expr.get_args e in
        if List.length args = 0 then []
        else List.concat (List.map aux args)
    in
    remove_dups (aux form)

  let transform_vars tr = 
    let variables = ST.fold (fun _ term acc -> (get_vars_form term) @ acc) tr.transform [] in
    remove_dups variables

  let transform_vars_all tr = 
    let variables = ST.fold (fun v term acc -> v :: (get_vars_form term) @ acc) tr.transform [] in
    remove_dups variables

  let get_vars tr = 
    let tr_vars = transform_vars_all tr in
    let guard_vars = get_vars_form tr.guard in
    List.filter (fun v -> List.mem v !vars) (remove_dups (tr_vars @ guard_vars))

  (*Eliminate quantifiers from a formula using Z3 tactic "qe-light". Can get expensive.*)
  let elim_quantifiers_form_light form = 
    let qe_light = Z3.Tactic.mk_tactic ctx "qe-light" in
    let g = Z3.Goal.mk_goal ctx false false false in
    Z3.Goal.add g [form];
    let ar = Z3.Tactic.apply qe_light g None in
    let sgs = Z3.Tactic.ApplyResult.get_subgoals ar in
    Z3.Expr.simplify (Z3.Boolean.mk_and ctx (List.map Z3.Goal.as_expr sgs)) None

  (*Eliminate quantifiers from a formula using Z3 tactic "qe". Can get expensive.*)
  let elim_quantifiers_form form = 
    let qe = Z3.Tactic.mk_tactic ctx "qe" in
    let g = Z3.Goal.mk_goal ctx false false false in
    Z3.Goal.add g [form];
    let ar = Z3.Tactic.apply qe g None in
    let sgs = Z3.Tactic.ApplyResult.get_subgoals ar in
    Z3.Expr.simplify (Z3.Boolean.mk_and ctx (List.map Z3.Goal.as_expr sgs)) None

  (*Quantify out a set of variables*)
  let project_vars form variables = 
    let var_symbols = List.map (fun v -> Z3.Symbol.mk_string ctx v) variables in
    let bound_vars = List.mapi (fun i _ -> Z3.Quantifier.mk_bound ctx i (Z3.Arithmetic.Integer.mk_sort ctx)) var_symbols in
    let subst_form = Z3.Expr.substitute form (List.map (Z3.Arithmetic.Integer.mk_const ctx) var_symbols) bound_vars in
    let quant = Z3.Quantifier.mk_exists ctx (List.map (fun _ -> Z3.Arithmetic.Integer.mk_sort ctx) bound_vars) var_symbols subst_form None [] [] None None in
    Z3.Expr.simplify (Z3.Quantifier.expr_of_quantifier quant) None

  (*Eliminate skolem vars that are only in the guard.*)
  let elim_skolem_light tr = 
    let tr_vars = transform_vars tr in
    let guard_vars = get_vars_form tr.guard in
    let (_, vars_to_remove) = List.partition (fun v -> List.mem v tr_vars || List.mem v !vars) guard_vars in
    let projected_guard = project_vars tr.guard vars_to_remove in
    {transform = tr.transform; guard = elim_quantifiers_form_light projected_guard}
  
  (*Same as previous but with full qe.*)
  let elim_skolem tr = 
    let tr_vars = transform_vars tr in
    let guard_vars = get_vars_form tr.guard in
    let (_, vars_to_remove) = List.partition (fun v -> List.mem v tr_vars || List.mem v !vars) guard_vars in
    let projected_guard = project_vars tr.guard vars_to_remove in
    {transform = tr.transform; guard = elim_quantifiers_form projected_guard}

  (*These might not actually simplify the transition. qe might make things messier.*)
  let simplify tr =
    elim_skolem tr

  let simplify_light tr =
    elim_skolem_light tr

  (*It is possible when combining formulas that names may clash. So this takes all non-program variables and creates fresh copies.*)
  let make_fresh_skolems tr = 
    let tr_vars =  transform_vars tr in
    let guard_vars = get_vars_form tr.guard in
    let skolem_vars = List.filter (fun v -> not (List.mem v !vars)) (remove_dups (tr_vars @ guard_vars)) in
    let make_fresh_var var =
      if ST.mem var !phi_vars then mk_phi (ST.find var !phi_vars)
      else if ST.mem var !var_skolems then make_fresh (ST.find var !var_skolems)
      else if List.mem var !loop_counters then make_loop_counter ()
      else
        make_havoc ()
    in
    let fresh_vars = List.map make_fresh_var skolem_vars in
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
          (*In this case the left transition and the right transition for v will give different values.
            So we introduce a phi var for the transform and set the left and right transforms equal to phi
            and put in the guard with a disjunction.*)
          let phi = mk_phi v in
          let left_term = 
            match x with 
            | Some s -> s
            | None -> Z3.Arithmetic.Integer.mk_const_s ctx v in
          let right_term = 
            match y with 
            | Some t -> t
            | None -> Z3.Arithmetic.Integer.mk_const_s ctx v in
          left_eq := (Z3.Boolean.mk_eq ctx left_term phi) :: (!left_eq);
          right_eq := (Z3.Boolean.mk_eq ctx right_term phi) :: (!right_eq);
          Some phi
      in
      ST.merge merge left.transform right.transform
    in
    let guard = Z3.Boolean.mk_or ctx [(Z3.Boolean.mk_and ctx (left.guard :: (!left_eq))); (Z3.Boolean.mk_and ctx (right.guard :: (!right_eq)))] in
    {transform; guard}

  (*To extend we must substitute the rhs of the transforms of x in for the pre-state program vars of y.*)
  let mul x y =
    let y = make_fresh_skolems y in
    let left_subst = 
      let sub_pairs = ref [] in
      ST.iter (fun v term -> sub_pairs := (Z3.Arithmetic.Integer.mk_const_s ctx v, term) :: !sub_pairs) x.transform;
      List.split !sub_pairs
    in
    let guard = Z3.Boolean.mk_and ctx [x.guard; Z3.Expr.substitute y.guard (fst left_subst) (snd left_subst)] in
    let merge v left right = 
      let new_exp = Z3.Expr.substitute right [Z3.Arithmetic.Integer.mk_const_s ctx v] [left] in
      Some new_exp
    in
    let transform = ST.union merge x.transform y.transform in
    {transform; guard}

  (*The next few functions interpret types from Sigs as transitions.*)
  let interp_term a = 
    match a with 
    | Int n -> Z3.Arithmetic.Integer.mk_numeral_i ctx n
    | Times (n, v) -> Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx n; Z3.Arithmetic.Integer.mk_const_s ctx v]

  let interp_exp (Add l) = 
    if List.length l = 0 then Z3.Arithmetic.Integer.mk_numeral_i ctx 0
    else if List.length l = 1 then interp_term (List.hd l)
    else Z3.Arithmetic.mk_add ctx (List.map interp_term l)

  let interp_aexp x = 
    match x with
    | Sum xp -> interp_exp xp
    | Mod (y, z) -> Z3.Arithmetic.Integer.mk_mod ctx (interp_exp y) (interp_exp z)

  let interp_p p = 
    match p with
    | LessEq(x, y) -> Z3.Arithmetic.mk_le ctx (interp_aexp x) (interp_aexp y)
    | Less (x, y) -> Z3.Arithmetic.mk_lt ctx (interp_aexp x) (interp_aexp y)
    | GreaterEq (x, y) -> Z3.Arithmetic.mk_ge ctx (interp_aexp x) (interp_aexp y)
    | Greater (x, y) -> Z3.Arithmetic.mk_gt ctx (interp_aexp x) (interp_aexp y)
    | Eq (x, y) -> Z3.Boolean.mk_eq ctx (interp_aexp x) (interp_aexp y)

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

  (*To check and assertion we negate the assertion and check for unsatisfiability.*)
  let check_assert summary asser = 
    let assertion = interp_c asser in
    let transform = summary.transform in
    let guard = summary.guard in
    let subst = 
      let sub_pairs = ref [] in
      ST.iter (fun v term -> sub_pairs := (Z3.Arithmetic.Integer.mk_const_s ctx v, term) :: !sub_pairs) transform;
      List.split !sub_pairs
    in
    let sub_assert = Z3.Expr.substitute assertion (fst subst) (snd subst) in
    let final_check = Z3.Boolean.mk_and ctx [guard; Z3.Boolean.mk_not ctx sub_assert] in
    let solver = Z3.Solver.mk_simple_solver ctx in
    Z3.Solver.add solver [final_check];
    match (Z3.Solver.check solver []) with
    | Z3.Solver.UNSATISFIABLE -> true
    | _ -> false

  (*The pre-state of a transition is the guard. Simplifying here tends to make the output more readible.*)
  let get_pre tr = 
    simplify {transform = ST.empty; guard = tr.guard}

  (*To get the post-state we substitute fresh skolem vars for each pre-state var, and then set the pre-state vars
    equal to the new fresh skolem var. *)
    let get_post tr =
      let tr_vars = get_vars tr in
      let transform = ref ST.empty in
      let make_fresh_sub (sub_pairs) var =
        let fresh = make_fresh var in
        (*transform := ST.add var fresh !transform;*)
        let var_const = Z3.Arithmetic.Integer.mk_const_s ctx var in
        let subst = (var_const, fresh) in
        (try
          let new_term = Z3.Expr.substitute (ST.find var tr.transform) [var_const] [fresh] in
          transform := ST.add var new_term !transform
        with Not_found ->
          transform := ST.add var fresh !transform);
        (subst :: sub_pairs)
      in
      let (sub_pairs) = List.fold_left make_fresh_sub ([]) tr_vars in
      let (var_const, fresh_vars) = List.split sub_pairs in
      let proj_guard = Z3.Expr.substitute tr.guard var_const fresh_vars in
      simplify {transform = !transform; guard = proj_guard}

  let meet a b =
    let a_fresh = make_fresh_skolems a in
    let new_eqs = ref [] in
    let merge_transorms var a_tr b_tr =
      let new_skol = make_fresh var in
      new_eqs := (Z3.Boolean.mk_eq ctx new_skol a_tr) :: (Z3.Boolean.mk_eq ctx new_skol b_tr) :: !new_eqs;
      Some new_skol
    in
    let a_transform = a_fresh.transform in
    let b_transform = b.transform in
    let new_transform = ST.union merge_transorms a_transform b_transform in
    simplify {transform = new_transform; guard = Z3.Boolean.mk_and ctx (a_fresh.guard :: b.guard :: !new_eqs)}

  let neg_pre tr = 
    {transform = ST.empty; guard = Z3.Boolean.mk_not ctx tr.guard}

  (*Convert a transition to a transition formula by setting prime variables equal to transforms if available or x'=x otherwise.
    We then project out any other variable that is not a program or prime variable.*)
  let to_formula tr =
    let transform = tr.transform in
    let guard = tr.guard in
    let transforms = List.map
      (fun v -> 
        let vp = Z3.Arithmetic.Integer.mk_const_s ctx (ST.find v !prime_table) in
        try 
          let term = ST.find v transform in
          Z3.Boolean.mk_eq ctx vp term
        with Not_found -> 
          Z3.Boolean.mk_eq ctx vp (Z3.Arithmetic.Integer.mk_const_s ctx v)
        ) !vars in
    let form = (Z3.Boolean.mk_and ctx (guard :: transforms)) in
    let form_vars = get_vars_form form in
    let p_vars = List.map (fun v -> ST.find v !prime_table) !vars in
    let skolem_vars = List.filter (fun v -> (not (List.mem v !vars)) && not (List.mem v p_vars)) form_vars in
    (project_vars form skolem_vars, ctx)

  let to_string tr = 
    let tr_simp = tr in
    let transform = tr_simp.transform in
    let transform_list = ref [] in
    ST.iter (fun v term -> 
      transform_list := (v ^ "' := " ^ (Z3.Expr.to_string (Z3.Expr.simplify term None)))::!transform_list) transform;
    let transform_str = String.concat "\n" !transform_list in
    let guard_str = Z3.Expr.to_string (Z3.Expr.simplify tr_simp.guard None) in
    transform_str ^ "\nwhen " ^ guard_str

  (*Convert a recurrence solution to a transition.*)
  let rec_sol_to_tr rec_sol loop_vars = 
    match rec_sol with
    | EmptySol -> 
      (*If the solution is empty, we know nothing and must havoc all the loop_vars.*)
      let transform = ref ST.empty in
      List.iter (fun v -> transform := ST.add v (make_havoc ()) !transform) loop_vars;
      let guard = Z3.Boolean.mk_true ctx in
      {transform = !transform; guard}
    | InfeasibleSol ->
      {transform = ST.empty; guard = Z3.Boolean.mk_false ctx}
    | RecsSol sols ->
      let loop_counter = make_loop_counter () in
      let summarized_vars = ref [] in
      let folder acc (RecSol (Term (Add rec_terms), Times(inc, K))) = 
        let process_var_term (trans, lhs_terms, rhs_terms) vt = 
          match vt with
          | (Times (coef, var)) ->
            summarized_vars := var :: !summarized_vars;
            let fresh = make_fresh var in
            let lhs_term = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx coef; fresh] in
            let rhs_term = Z3.Arithmetic.mk_mul ctx [Z3.Arithmetic.Integer.mk_numeral_i ctx coef; Z3.Arithmetic.Integer.mk_const_s ctx var] in
            (ST.add var fresh trans, lhs_term :: lhs_terms, rhs_term :: rhs_terms)
          | _ -> failwith "A rec term should not have a constant integer in it."
        in
        let (transf, lhs_terms, rhs_terms) = List.fold_left process_var_term (fst acc, [], []) rec_terms in
        let increase = Z3.Arithmetic.mk_mul ctx [loop_counter; Z3.Arithmetic.Integer.mk_numeral_i ctx inc] in
        let lhs = Z3.Arithmetic.mk_add ctx lhs_terms in
        let rhs = Z3.Arithmetic.mk_add ctx (increase :: rhs_terms) in
        (transf, (Z3.Boolean.mk_eq ctx lhs rhs) :: snd acc)
      in
      let (transform, guards) = List.fold_left folder (ST.empty, []) sols in
      let loop_counter_ge_1 = Z3.Arithmetic.mk_ge ctx loop_counter (Z3.Arithmetic.Integer.mk_numeral_i ctx 1) in
      let unsummarized_vars = List.filter (fun v -> not (List.mem v !summarized_vars)) loop_vars in
      (*We must havoc any variable that was not summarized in the solution.*)
      let transform_havoc = List.fold_left (fun acc havoc_var -> ST.add havoc_var (make_havoc ()) acc) transform unsummarized_vars in
      {transform = transform_havoc; guard = Z3.Boolean.mk_and ctx (loop_counter_ge_1 :: guards)}
