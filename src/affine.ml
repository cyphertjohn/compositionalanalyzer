module Logger = Log

module Make (A : Sigs.Rational) = struct

  open A

  module MA = Matrix.Make(A)

  open MA

  module S = Map.Make(String)

  type t = 
    | Bot
    | Top
    | I of A.q S.t array * A.q array * string list

  let to_matrix coef_map vars = 
    let n = List.length vars in
    let build_row row_map = 
      let row = Array.make n (from_string "0") in
      List.iteri 
        (fun i var -> 
          try 
            row.(i) <- S.find var row_map
          with Not_found ->
            ()
        ) vars;
      row
    in
    Array.map build_row coef_map

  let to_map matrix vars =
    let var_pos = List.mapi (fun i v -> (i, v)) vars in
    let build_row row = 
      let folder acc (i, v) =
        if is_zero row.(i) then acc
        else S.add v row.(i) acc
      in
      List.fold_left folder S.empty var_pos
    in
    Array.map build_row matrix


  let to_string d =
    match d with
    | Bot -> "Bot"
    | Top -> "Top"
    | I (map, b, vars) -> 
      let row i coef_map = 
        if S.cardinal coef_map = 0 then "0 == " ^ (to_string b.(i))
        else 
          let term_list = (S.fold (fun var coef acc -> ((to_string coef) ^ var) :: acc) coef_map []) in
          (String.concat "+" term_list) ^ " == " ^ (to_string b.(i))
      in
      String.concat "\n" (Array.to_list (Array.mapi row map))
    
  let merge_vars vl1 vl2 =
    let (sortvl1, sortvl2) = (List.sort compare vl1, List.sort compare vl2) in
    let rec aux l1 l2 acc =
      match (l1, l2) with
      | ([], _) -> l2 @ acc
      | (_, []) -> l1 @ acc
      | (x :: xs, y :: ys) -> 
        if compare x y = 0 then aux xs ys (x :: acc)
        else if compare x y < 0 then aux xs l2 (x :: acc)
        else aux l1 ys (y :: acc)
      in
    aux sortvl1 sortvl2 []

  let project el vars_to_keep = 
    match el with
    | Top -> Top
    | Bot -> Bot
    | I (m, b, vars) ->
      let (_, vars_to_remove) = List.partition (fun v -> List.mem v vars_to_keep) vars in
      let a = to_matrix m (vars_to_remove @ vars_to_keep) in
      let a_neg_b = Array.mapi (fun i arow -> Array.append arow (Array.make 1 (neg b.(i)))) a in
      let (r, _, _) = rref a_neg_b in
      let keep_row row = 
        let leading_zeros = Array.for_all is_zero (Array.sub row 0 (List.length vars_to_remove)) in
        if leading_zeros then
          not (Array.for_all is_zero row)
        else
          leading_zeros
      in
      let proj_rows = List.map (fun row -> Array.sub row (List.length vars_to_remove) ((List.length vars_to_keep) + 1)) (List.filter keep_row (Array.to_list r)) in
      let proj_rows = List.map normalize proj_rows in
      let (new_a, new_b) = List.split (List.map (fun row -> (Array.sub row 0 (List.length vars_to_keep), neg row.(List.length vars_to_keep))) proj_rows) in
      I (to_map (Array.of_list new_a) vars_to_keep, Array.of_list new_b, vars_to_keep)

  
  let bot = Bot

  let join x y = 
    match (x, y) with
    | (Top, _) -> Top
    | (_, Top) -> Top
    | (Bot, _) -> y
    | (_, Bot) -> x
    | (I (m1, b1, vars1), I (m2, b2, vars2)) -> 
      let vars = merge_vars vars1 vars2 in
      let a1 = to_matrix m1 vars in
      let a2 = to_matrix m2 vars in
      let first_row_block = Array.make_matrix 1 (3 + 3*(List.length vars)) (from_string "0") in
      first_row_block.(0).(0) <- from_string "1";
      first_row_block.(0).(1) <- from_string "1";
      first_row_block.(0).((Array.length first_row_block.(0)) - 1) <- from_string "-1";
      let construct_a1_block i _ = 
        let bblock = Array.make (2) (from_string "0") in
        bblock.(0) <- neg b1.(i);
        let zeros = Array.make (1 + 2*(List.length vars)) (from_string "0") in
        Array.append bblock (Array.append a1.(i) zeros)
      in
      let construct_a2_block i _ = 
        let bblock = Array.make 2 (from_string "0") in
        bblock.(1) <- neg b2.(i);
        let zeros1 = Array.make (List.length vars) (from_string "0") in
        let zeros2 = Array.make (1+(List.length vars)) (from_string "0") in
        Array.append bblock (Array.append zeros1 (Array.append a2.(i) zeros2))
      in
      let neg_iden = make_iden ~neg:(true) (List.length vars) in
      let iden = make_iden (List.length vars) in
      let construct_iden_block i _ =
        let bblock = Array.make 2 (from_string "0") in
        let zero = Array.make 1 (from_string "0") in
        Array.append bblock (Array.append neg_iden.(i) (Array.append neg_iden.(i) (Array.append iden.(i) zero)))
      in
      let a1_block = Array.mapi construct_a1_block a1 in
      let a2_block = Array.mapi construct_a2_block a2 in
      let iden_block = Array.mapi construct_iden_block iden in
      let cons_matrix = Array.append first_row_block (Array.append a1_block (Array.append a2_block iden_block)) in
      let (r, _, _) = rref cons_matrix in
      let keep_row row = 
        let leading_zeros = Array.for_all is_zero (Array.sub row 0 (2 + 2*(List.length vars))) in
        if leading_zeros then
          not (Array.for_all is_zero row)
        else
          leading_zeros
      in
      let new_const = List.map (fun row -> Array.sub row (2+2*(List.length vars)) ((List.length vars) + 1)) (List.filter keep_row (Array.to_list r)) in
      let new_const = List.map normalize new_const in
      if List.length new_const = 0 then Top
      else
        let new_a = Array.of_list (List.map (fun row -> Array.sub row 0 (List.length vars)) new_const) in
        let new_am = to_map new_a vars in
        let new_b = Array.of_list (List.map (fun row -> neg row.((Array.length row) - 1)) new_const) in
        I (new_am, new_b, vars)

  let eqs_to_t l =
    let eq_to_t (Sigs.Expr.Equal (lhs, rhs)) = 
      let add_term_to_map is_rhs coef var map =
        try
          let old_coef = S.find var map in
          let new_coef = 
            if is_rhs then add old_coef (neg (from_string (string_of_int coef)))
            else add old_coef (from_string (string_of_int coef))
          in
          S.add var new_coef (S.remove var map)
        with Not_found ->
          if is_rhs then S.add var (neg (from_string (string_of_int coef))) map
          else S.add var (from_string (string_of_int coef)) map
      in
      let process_term is_rhs ((map, const), variables) term = 
        match term with
        | Sigs.Expr.Int n -> 
          let n_q = from_string (string_of_int n) in
          if is_rhs then
            ((map, add n_q const), variables)
          else
            ((map, add (neg n_q) const), variables)
        | Sigs.Expr.Times (coef, var) ->
          let new_map = add_term_to_map is_rhs coef var map in
          let new_vars = 
            if List.mem var variables then variables
            else var :: variables
          in
          ((new_map, const), new_vars)
      in
      let Sigs.Expr.Add l = lhs in
      let ((lhs_map, const), lvariables) = List.fold_left (process_term false) ((S.empty, from_string "0"), []) l in
      let Sigs.Expr.Add r = rhs in
      List.fold_left (process_term true) ((lhs_map, const), lvariables) r
    in
    let (m, vars) = List.split (List.map eq_to_t l) in
    let variables = List.fold_left merge_vars [] vars in
    let (rows, consts) = List.split m in    
    I (Array.of_list rows, Array.of_list consts, variables)
      
  let meet x y =
    match (x, y) with
    | (Bot, _) -> Bot
    | (_, Bot) -> Bot
    | (Top, _) -> y
    | (_, Top) -> x
    | (I(m1, b1, vars1), I(m2, b2, vars2)) ->
      let variables = merge_vars vars1 vars2 in
      let a1 = to_matrix m1 variables in
      let a2 = to_matrix m2 variables in
      let make_row i _ =
        if i < Array.length a1 then Array.append (a1.(i)) (Array.make 1 (neg b1.(i)))
        else Array.append (a2.(i-(Array.length a1))) (Array.make 1 (neg b2.(i-(Array.length a1))))
      in
      let combined_mat = Array.mapi make_row (Array.make ((Array.length a1) + (Array.length a2)) 0) in
      let (r, _, _) = rref combined_mat in
      let r_l = Array.to_list r in
      let zeros_removed = List.filter (fun row -> not (Array.for_all is_zero row)) r_l in
      let zeros_removed = List.map normalize zeros_removed in
      if List.length zeros_removed = 0 then Top
      else 
        let new_mat_l = List.map (fun row -> Array.sub row 0 (List.length variables), neg row.(List.length variables)) zeros_removed in    
        let incon = List.exists (fun (row, b) -> Array.for_all is_zero row && not (is_zero b)) new_mat_l in
        if incon then Bot
        else 
          let (new_a_l, new_b_l) = List.split new_mat_l in
          let new_m = to_map (Array.of_list new_a_l) variables in
          I (new_m, Array.of_list new_b_l, variables)
  

  let add_eqs (aff : t) (l : Sigs.Expr.lineq list) = 
    meet aff (eqs_to_t l)

  let sing model = 
    let decs = Z3.Model.get_decls model in
    let vars = List.map (fun dec -> Z3.Symbol.get_string (Z3.FuncDecl.get_name dec)) decs in
    let make_row dec = 
      match Z3.Model.get_const_interp model dec with
      | None -> failwith "Model without an interpretation"
      | Some m -> S.add (Z3.Symbol.get_string (Z3.FuncDecl.get_name dec)) (from_string "1") S.empty, from_string (Z3.Expr.to_string m)
    in
    let (alist, blist) = List.split (List.map make_row decs) in
    I (Array.of_list alist, Array.of_list blist, vars)


  let gamma_hat ctx x = 
    match x with
    | Top -> Z3.Boolean.mk_true ctx
    | Bot -> Z3.Boolean.mk_false ctx
    | I (m, b, vars) ->
      let z3a = Array.map (Array.map (fun v -> (Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string v)))) (to_matrix m vars) in
      let z3b = Array.map (fun v -> (Z3.Arithmetic.Integer.mk_numeral_s ctx (A.to_string v))) b in
      let make_exp_from_row i _ =
        let list_prod = List.map2 (fun var const -> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_const_s ctx var); const]) vars (Array.to_list z3a.(i)) in
        let lhs = Z3.Arithmetic.mk_add ctx list_prod in
        Z3.Boolean.mk_eq ctx lhs z3b.(i)
      in
      Z3.Boolean.mk_and ctx (Array.to_list (Array.mapi make_exp_from_row z3a))

end 

