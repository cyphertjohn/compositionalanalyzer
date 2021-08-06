module Make (A : Sigs.Rational) = struct

  open A
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
    let lcm = Array.fold_left lcm (z_of_string "1") denoms in
    Array.map (mul (z_to_q lcm)) l

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
            let negated = neg a.(curr).(!curr_col) in
            let new_row = add_rows (mult_const negated (a.(!curr_row))) (a.(curr)) in
            let new_row_t = add_rows (mult_const negated (t.(!curr_row))) (t.(curr)) in
            t.(curr) <- new_row_t;
            a.(curr) <- new_row)
        done;
        curr_row := !curr_row + 1;);
      curr_col := !curr_col + 1;
    done;
    (a, t, p)

  let get_mat_widths mat = 
    let n = Array.length mat.(0) in
    Array.fold_left (fun acc row -> Array.map2 (fun x y -> max x y) acc row) (Array.make n (-1)) (Array.map (fun row -> Array.map (fun v -> String.length (A.to_string v)) row) mat) 
      
  let mat_row_to_string row widths = 
    let el_to_string i v = 
      let val_str = A.to_string v in
      let val_len = String.length val_str in
      let spaces = String.make (widths.(i) - val_len) ' ' in
      if i = 0 then spaces ^ val_str
        else " " ^ spaces ^ val_str
      in
      "|" ^ (String.concat " " (Array.to_list (Array.mapi el_to_string row))) ^ "|" 
  
  let matrix_to_string m = 
    let mw = get_mat_widths m in
    String.concat "\n" (Array.to_list (Array.mapi (fun i _ -> mat_row_to_string m.(i) mw) m))

  
end