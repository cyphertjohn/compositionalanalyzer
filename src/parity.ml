module Logger = Log


module S = Map.Make(String)

(*The parity of a variable is either Even, Odd, or Unknown.*)
type parity = 
  | Even
  | Odd
  | Unknown

let parity_to_string p =
  match p with
  | Even -> "Even"
  | Odd -> "Odd"
  | Unknown -> "?"

(*An element of this domain is either Bot, Top meaning we know nothing, or a map from variables to parities.*)
type t = 
  | Bot (*<--Usually means infeasible.*)
  | Top (*<--We know nothing.*)
  | I of parity S.t (*S.t is a map from strings (variable names) to parity*)


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

(* This function takes a model, an assignment of variables to values, and computes a domain element from that.
Essentially, we just look up whether each variable is given an even or add value and give it that parity as our
domain element. *)
let sing (model : Z3.Model.model) = 
  let int_from_z3_string s = (* This is needed due to a peculiarity of how Z3 prints integers.*)
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
      if (m_as_int mod 2) = 0 then S.add var Even acc (*check if the assignment is even or odd*)
      else S.add var Odd acc
  in
  I (List.fold_left folder S.empty decs)

(*Gamma hat converts a domain element to a formula. Here it's pretty simple. If a variable is assigned even then we add the constraint x mod 2 == 0.
  Similar for odd. If the parity of the variable is unknown we don't add a constraint. For the most part gamma_hat is just the formula representation
  of the domain element.*)
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


(* This function implements the greatest lower bound of two elements in the domain lattice. *)
let meet x y =
  match (x, y) with
  | (Bot, _) -> Bot (*Bot meet anything is Bot*)
  | (_, Bot) -> Bot
  | (Top, _) -> y (*Top meet y is y*)
  | (_, Top) -> x
  | (I pmap1, I pmap2) -> 
    let incon = ref false in
    let meet_var (v : string) (p1 : parity) (p2 : parity) = (*This function is called is v is present in both pmaps. In which case we must decide what to do.*)
      match (p1, p2) with
      | (Unknown, _) -> Some p2 (*The lower bound between Unknown and any p2 is p2*)
      | (_, Unknown) -> Some p1
      | (a, b) when a = b -> Some p1 (*If v has the same parity in both domain elements then we retain that info *)
      | _ -> incon := true; None  (*If this case happens we must have that v was Odd in one map and Even in the other. This is inconsistent. *)
    in
    let res = S.union meet_var pmap1 pmap2 in
    if !incon then Bot
    else I res

(* Compute the least upper bound of two domain elements in the domain lattice. *)
let join (x : t) (y : t) = 
  match (x, y) with
  | (Top, _) -> Top (*Top join anything is Top *)
  | (_, Top) -> Top
  | (Bot, _) -> y (*Bot join y is y*)
  | (_, Bot) -> x
  | (I pmap1, I pmap2) ->  (*If x and y map some vars to parities, we must merge the maps.*)

    (*TODO: Currently this says that if we have that v had some parity p1 in x and some parity p2 in y, we always return unknown.
      This is sound, but we throw away too much information. Can you fix this function? Hint: what should the parity of v be
      if p1 is the same as p2? What if they're different? Should only take 1 or 2 lines to fix. Also the resulting parity needs to be
      wrapped with a "Some". *)
    let join_var (v : string) (p1 : parity) (p2 : parity) : parity option = 
      (*Replace this function body with your code*)
      Some Unknown 
    in
    let combined = S.union join_var pmap1 pmap2 in
    if is_top (I combined) then Top
    else I combined