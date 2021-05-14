module type Domain = sig
  type t
  val bot : t
  val sing : Z3.context -> Z3.Model.model -> t
  val join : t -> t -> t
  val gamma_hat : Z3.context -> t -> Z3.Expr.expr
  val to_string : t -> string
end

module Make () = struct

  let add = Mpqf.add 
  let mul = Mpqf.mul
  let div = Mpqf.div
  let is_zero c = (Mpqf.cmp_int c 0) = 0
  let is_one c = (Mpqf.cmp_int c 1) = 0
  let to_string_c = Mpqf.to_string
  let from_string s =
    let s = 
      if (s.[0] = '(') then
        let trimmed = String.sub s 1 ((String.length s) - 2) in
        String.concat "" (String.split_on_char ' ' trimmed)
      else s
    in
    Mpqf.of_string s
  
  let cmp = Mpqf.cmp
  let get_den = Mpqf.get_den

  let vars = ref []

  let set_vars variables = vars := variables

  let make_iden ?neg:(negate = false) n = 
    let res = Array.make n 0 in
    let row i _ =
      let r = Array.make n (from_string "0") in
      (if negate then
        r.(i) <- from_string "-1"
      else
        r.(i) <- from_string "1");
      r
    in
    Array.mapi row res
  
  let normalize l = 
    let denoms = Array.map get_den l in
    let lcm = Array.fold_left Mpzf.lcm (Mpzf.of_string "1") denoms in
    Array.map (mul (Mpqf.of_mpz lcm)) l


  let neg = mul (from_string "-1")

  let mat_mult a b =
    let m = Array.length a in
    let n = Array.length a.(0) in
    let p = Array.length b in
    let q = Array.length b.(0) in
    if p = n then (
      let res = Array.make_matrix m q (from_string "0") in
      for i = 0 to (m-1) do
        for j = 0 to (q-1) do
          let colj = Array.make n (from_string "0") in
          for k = 0 to (n-1) do
            colj.(k) <- b.(k).(j)
          done;
          res.(i).(j) <- Array.fold_left add (from_string "0") (Array.map2 (fun x y -> mul x y) a.(i) colj)
        done
      done;
      res
    )
    else failwith "incompatible matrix dimensions"

  let mat_mult_v a v =
    Array.map (fun row -> Array.fold_left add (from_string "0") (Array.map2 mul row v)) a
  
  let rref b = 
    let get_pivot mat curr_c curr_r = 
      let matList = Array.to_list mat in
      let pivot_test k row = if not (is_zero row.(curr_c)) && k >= curr_r then k else -1 in
      match (List.find_opt ((<=) 0) (List.mapi pivot_test matList)) with
      | None -> -1
      | Some v -> v
    in 
    let swap l p mat = 
      let temp = mat.(l) in
      mat.(l) <- mat.(p);
      mat.(p) <- temp
    in
    let mult_const c row = Array.map (mul c) row in
    let add_rows = Array.map2 add in
    let a = Array.copy b in
    let m = Array.length a in
    let n = Array.length a.(0) in
    let t = Array.make_matrix m m (from_string "0") in
    let p = Array.make_matrix m m (from_string "0") in
    for j = 0 to m-1 do
      t.(j).(j) <- from_string "1";
      p.(j).(j) <- from_string "1"
    done;
    let curr_row = ref 0 in
    let curr_col = ref 0 in
    while !curr_row < m && !curr_col < n do
      let pivot = get_pivot a !curr_col !curr_row in
      if pivot >= 0 then (
        if pivot <> !curr_row then (swap pivot !curr_row a; swap pivot !curr_row t; swap pivot !curr_row p);
        let head_value = a.(!curr_row).(!curr_col) in
        a.(!curr_row) <- mult_const (div (from_string "1") head_value) a.(!curr_row);
        t.(!curr_row) <- mult_const (div (from_string "1") head_value) t.(!curr_row);
        for curr = 0 to m - 1 do
          if not (is_zero a.(curr).(!curr_col)) && curr <> !curr_row then (
            let neg = mul (from_string "-1") a.(curr).(!curr_col) in
            let new_row = add_rows (mult_const neg (a.(!curr_row))) (a.(curr)) in
            let new_row_t = add_rows (mult_const neg (t.(!curr_row))) (t.(curr)) in
            t.(curr) <- new_row_t;
            a.(curr) <- new_row)
        done;
        curr_row := !curr_row + 1;);
      curr_col := !curr_col + 1;
    done;
    (a, t, p)
  
  type t = 
    | Bot
    | Top
    | I of Mpqf.t array array * Mpqf.t array

  let to_string d =
    match d with
    | Bot -> "Bot"
    | Top -> "Top"
    | I (a, b) -> 
      let row i = 
        String.concat "+" (List.map2 (fun var x -> (to_string_c x) ^ var) !vars (Array.to_list a.(i))) ^ " == " ^ (to_string_c b.(i))
      in
      String.concat "\n" (Array.to_list (Array.mapi (fun r _ -> row r) a))

  let get_mat_widths mat = 
    let n = Array.length mat.(0) in
    Array.fold_left (fun acc row -> Array.map2 (fun x y -> max x y) acc row) (Array.make n (-1)) (Array.map (fun row -> Array.map (fun v -> String.length (Mpqf.to_string v)) row) mat) 
    
  let mat_row_to_string row widths = 
    let el_to_string i v = 
      let val_str = Mpqf.to_string v in
      let val_len = String.length val_str in
      let spaces = String.make (widths.(i) - val_len) ' ' in
      if i = 0 then spaces ^ val_str
        else " " ^ spaces ^ val_str
      in
      "|" ^ (String.concat " " (Array.to_list (Array.mapi el_to_string row))) ^ "|" 

  let print_matrix m = 
    let mw = get_mat_widths m in
    print_endline (String.concat "\n" (Array.to_list (Array.mapi (fun i _ -> mat_row_to_string m.(i) mw) m)))
    
  let bot = Bot

  let join x y = 
    match (x, y) with
    | (Top, _) -> Top
    | (_, Top) -> Top
    | (Bot, _) -> y
    | (_, Bot) -> x
    | (I (a1, b1), I (a2, b2)) -> 
      let first_row_block = Array.make_matrix 1 (3 + 3*(List.length !vars)) (from_string "0") in
      first_row_block.(0).(0) <- from_string "1";
      first_row_block.(0).(1) <- from_string "1";
      first_row_block.(0).((Array.length first_row_block.(0)) - 1) <- from_string "-1";
      let construct_a1_block i _ = 
        let bblock = Array.make (2) (from_string "0") in
        bblock.(0) <- neg b1.(i);
        let zeros = Array.make (1 + 2*(List.length !vars)) (from_string "0") in
        Array.append bblock (Array.append a1.(i) zeros)
      in
      let construct_a2_block i _ = 
        let bblock = Array.make 2 (from_string "0") in
        bblock.(1) <- neg b2.(i);
        let zeros1 = Array.make (List.length !vars) (from_string "0") in
        let zeros2 = Array.make (1+(List.length !vars)) (from_string "0") in
        Array.append bblock (Array.append zeros1 (Array.append a2.(i) zeros2))
      in
      let neg_iden = make_iden ~neg:(true) (List.length !vars) in
      let iden = make_iden (List.length !vars) in
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
        let leading_zeros = Array.for_all is_zero (Array.sub row 0 (2 + 2*(List.length !vars))) in
        if leading_zeros then
          not (Array.for_all is_zero row)
        else
          leading_zeros
      in
      let new_const = List.map (fun row -> Array.sub row (2+2*(List.length !vars)) ((List.length !vars) + 1)) (List.filter keep_row (Array.to_list r)) in
      let new_const = List.map normalize new_const in
      if List.length new_const = 0 then Top
      else
        let new_a = Array.of_list (List.map (fun row -> Array.sub row 0 (List.length !vars)) new_const) in
        let new_b = Array.of_list (List.map (fun row -> neg row.((Array.length row) - 1)) new_const) in
        I (new_a, new_b)


  let sing ctx model = 
    let make_row i var = 
      let row = Array.make (List.length !vars) (from_string "0") in
      match (Z3.Model.get_const_interp_e model (Z3.Arithmetic.Integer.mk_const_s ctx var)) with
      | None -> (row, from_string "0")
      | Some e -> 
        row.(i) <- from_string "1";
        (row, from_string (Z3.Expr.to_string e))
    in
    let (alist, blist) = List.split (List.mapi make_row !vars) in
    I (Array.of_list alist, Array.of_list blist)


  let gamma_hat ctx x = 
    match x with
    | Top -> Z3.Boolean.mk_true ctx
    | Bot -> Z3.Boolean.mk_false ctx
    | I (a, b) ->
      let z3a = Array.map (Array.map (fun v -> (Z3.Arithmetic.Integer.mk_numeral_s ctx (Mpqf.to_string v)))) a in
      let z3b = Array.map (fun v -> (Z3.Arithmetic.Integer.mk_numeral_s ctx (Mpqf.to_string v))) b in
      let make_exp_from_row i _ =
        let list_prod = List.map2 (fun var const -> Z3.Arithmetic.mk_mul ctx [(Z3.Arithmetic.Integer.mk_const_s ctx var); const]) !vars (Array.to_list z3a.(i)) in
        let lhs = Z3.Arithmetic.mk_add ctx list_prod in
        Z3.Boolean.mk_eq ctx lhs z3b.(i)
      in
      Z3.Boolean.mk_and ctx (Array.to_list (Array.mapi make_exp_from_row a))

end 
