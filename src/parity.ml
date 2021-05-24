module Logger = Log


module S = Map.Make(String)

type parity = 
  | Even
  | Odd
  | Unknown

let parity_to_string p =
  match p with
  | Even -> "Even"
  | Odd -> "Odd"
  | Unknown -> "?"

type t = 
  | Bot
  | Top
  | I of parity S.t


let to_string d =
  match d with
  | Bot -> "Bot"
  | Top -> "Top"
  | I pmap -> 
    String.concat "\n" (S.fold (fun v p acc -> (v ^ "->" ^ (parity_to_string p)) :: acc) pmap [])
 
let bot = Bot

let is_top p = 
  match p with
  | Bot -> false
  | Top -> true
  | I pmap ->
    S.for_all (fun v p -> p=Unknown) pmap

let join x y = 
  match (x, y) with
  | (Top, _) -> Top
  | (_, Top) -> Top
  | (Bot, _) -> y
  | (_, Bot) -> x
  | (I pmap1, I pmap2) -> 
    let merge v p1 p2 = 
      if p1 = p2 then Some p1
      else Some Unknown
    in
    let combined = S.union merge pmap1 pmap2 in
    if is_top (I combined) then Top
    else I combined

let meet x y =
  match (x, y) with
  | (Bot, _) -> Bot
  | (_, Bot) -> Bot
  | (Top, _) -> y
  | (_, Top) -> x
  | (I pmap1, I pmap2) -> 
    let incon = ref false in
    let merge v p1 p2 =
      match (p1, p2) with
      | (Unknown, _) -> Some p2
      | (_, Unknown) -> Some p1
      | (a, b) when a = b -> Some p1
      | _ -> incon := true; None
    in
    let res = S.union merge pmap1 pmap2 in
    if !incon then Bot
    else I res

let sing model = 
  let int_from_z3_string s =
    let s = 
      if (s.[0]='(') then
        let trimmed = String.sub s 1 ((String.length s) - 2) in
        String.concat "" (String.split_on_char ' ' trimmed)
      else s
    in
    int_of_string s
  in
  let decs = Z3.Model.get_decls model in
  let folder acc dec = 
    match Z3.Model.get_const_interp model dec with
    | None -> failwith "Model without an interpretation"
    | Some m -> 
      let var = Z3.Symbol.get_string (Z3.FuncDecl.get_name dec) in
      let m_as_int = int_from_z3_string (Z3.Expr.to_string m) in
      if (m_as_int mod 2) = 0 then S.add var Even acc
      else S.add var Odd acc
  in
  I (List.fold_left folder S.empty decs)


let gamma_hat ctx x = 
  match x with
  | Top -> Z3.Boolean.mk_true ctx
  | Bot -> Z3.Boolean.mk_false ctx
  | I pmap ->
    let two = Z3.Arithmetic.Integer.mk_numeral_i ctx 2 in
    let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
    let folder v p acc =
      match p with 
      | Unknown -> acc
      | Even -> 
        let v_const = Z3.Arithmetic.Integer.mk_const_s ctx v in
        let v_even = Z3.Arithmetic.Integer.mk_mod ctx v_const two in
        (Z3.Boolean.mk_eq ctx v_even zero) :: acc
      | _ ->
        let v_const = Z3.Arithmetic.Integer.mk_const_s ctx v in
        let v_even = Z3.Arithmetic.Integer.mk_mod ctx v_const two in
        (Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx v_even zero)) :: acc
    in
    Z3.Boolean.mk_and ctx (S.fold folder pmap [])


