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

  
open Sigs.Recurrence
open Sigs.PathExp
open Sigs.Expr

include Transition

module A = Affine.Make(Q)

module ARA = Abstract.Make(A)

module S = Map.Make(String)

let extract_recs aff_eq loop_vars = 
  let delta_map = ref S.empty in
  let delta_vars = List.map (fun v -> delta_map := S.add ("d"^v) v !delta_map; "d"^v) loop_vars in
  let mk_delta_eq delta =
    let lhs = Add [Times (1, get_prime (S.find delta !delta_map)); Times (-1, S.find delta !delta_map)] in
    let rhs = Add [Times (1,  delta)] in
    Equal (lhs, rhs)
  in
  let delts_eq = List.map mk_delta_eq delta_vars in
  let extra_eqs = A.add_eqs aff_eq delts_eq in
  let delta_only = A.project extra_eqs delta_vars in
  match delta_only with
  | A.Top -> 
    Empty
  | A.Bot ->
    Infeasible
  | A.I(m, b, vars) -> 
    let extract_rec i row =
      let folder dvar coef acc =
        if Q.is_zero coef then acc
        else
          let var = S.find dvar !delta_map in
          let coef_i = int_of_string (Q.to_string coef) in
          Times (coef_i, var) :: acc
      in
      Rec (Term (Add (S.fold folder row [])), Inc (int_of_string (Q.to_string b.(i))))
    in
    Recs (Array.to_list (Array.mapi extract_rec m))

(*Rec (Term(x+y), Inc 5) -> (x+y)' = x+y + 5 -> (x+y)' = (x+y) + 5 * k*)    
let solve_rec (Rec (Term rec_term, Inc inc)) =
  RecSol (Term rec_term, Times (inc, K)) 

let solve_recs recurs = 
  match recurs with
  | Empty -> EmptySol
  | Infeasible -> InfeasibleSol
  | Recs recurrences -> RecsSol (List.map solve_rec recurrences)

let star tr = 
  let loop_vars = get_vars tr in
  let form, _ = to_formula tr in
  let pre = get_pre tr in
  let post = get_post tr in
  Logger.log_line ~level:`debug ("Pre:");
  Logger.log_line ~level:`debug (to_string pre);
  Logger.log_line ~level:`debug ("\nPost:");
  Logger.log_line ~level:`debug (to_string post);
  let aff_eq = ARA.alpha_from_below ctx form in
  let recs = extract_recs aff_eq loop_vars in
  Logger.log_line ("Loop Body Recs:");
  Logger.log_line (recs_to_string recs);
  Logger.log_line "";
  let sols = solve_recs recs in
  let some_iters = rec_sol_to_tr sols loop_vars in
  plus one (mul (mul pre some_iters) post)

let rec eval p = (*Could be memoized*)
  match p with
  | Letter a -> interp a
  | One -> one
  | Zero -> zero
  | Plus (a, b) -> plus (eval a) (eval b)
  | Mul (a, b) -> mul (eval a) (eval b)
  | Star a -> star (eval a)

let analyze_path_exp path_exp vars = 
  Logger.log_line (pathexp_to_string path_exp);
  set_prog_vars vars;
  let summary = eval path_exp in
  summary
    

let analyze_path_exp_assertion path_exp assertion vars = 
  let summary = analyze_path_exp path_exp vars in
  (summary, check_assert summary assertion)
