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

let extract_recs loop_body loop_vars = 
  let (loop_body_form, _) = to_formula loop_body in

  (*Extract the best set of affine equalities implied by the loop.*)
  let aff_eq = ARA.alpha_from_below ctx loop_body_form in
  let delta_map = ref S.empty in
  let delta_vars = List.map (fun v -> delta_map := S.add ("d"^v) v !delta_map; "d"^v) loop_vars in
  (*Create the delta equations*)
  let mk_delta_eq delta =
    let lhs = Add [Times (1, get_prime (S.find delta !delta_map)); Times (-1, S.find delta !delta_map)] in
    let rhs = Add [Times (1,  delta)] in
    Equal (lhs, rhs)
  in
  let delts_eq = List.map mk_delta_eq delta_vars in
  (*Add the delta equation to the previous set of extracted equalities.*)
  let extra_eqs = A.add_eqs aff_eq delts_eq in
  (*Project to only delta variables.*)
  let delta_only = A.project extra_eqs delta_vars in
  match delta_only with
  | A.Top -> 
    Empty
  | A.Bot ->
    Infeasible
  | A.I(m, b, _) -> 
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


(*Rec (Term(x+y), Inc 5)               <- Example input as type Sigs.Recurrence.lin_rec
  (x+y)_[k+1] = (x+y)_[k] + 5          <- The recurrence it corresponds to
  (x+y)' = (x+y) + 5 * k               <- The recurrence solution
  RecSol (Term (x+y), Times (5, K))    <- The solution as the type Sigs.Recurrence.lin_rec_sol
  *)
let solve_rec ((Rec (Term rec_term, Inc inc)) : lin_rec) : lin_rec_sol =
  RecSol(Term rec_term, Times (inc, K))

let solve_recs recurs = 
  match recurs with
  | Empty -> EmptySol
  | Infeasible -> InfeasibleSol
  | Recs recurrences -> RecsSol (List.map solve_rec recurrences)

let star loop_body = 
  let loop_pre_condition = get_pre loop_body in
  let loop_post_condition = get_post loop_body in
  Logger.log_line ~level:`trace ("Loop Pre:");
  Logger.log_line ~level:`trace (to_string loop_pre_condition);
  Logger.log_line ~level:`trace "";
  Logger.log_line ~level:`trace ("Loop Post:");
  Logger.log_line ~level:`trace (to_string loop_post_condition);
  Logger.log_line ~level:`trace "";
  let pre_post_transition = mul loop_pre_condition loop_post_condition in
  
  (* Uncomment the following block and run with debug to output
     extracted recurrence relations.
  *)
  
  let loop_vars = get_vars loop_body in 
  let recs = extract_recs loop_body loop_vars in
  Logger.log_line ~level:`debug ("Loop Body Recs:");
  Logger.log_line ~level:`debug (recs_to_string recs);
  Logger.log_line ~level:`debug "";
  

  (* Uncomment the following block for exercise 5.*)
  
  let sols = solve_recs recs in
  let some_iters = rec_sol_to_tr sols loop_vars in
  plus one (meet pre_post_transition some_iters)
  
  
  (*plus one pre_post_transition *)(*Comment out this line for exercise 5*)

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


let analyze_file in_file_name =
  let ic = open_in in_file_name in 
  let ((body, assertion), vars) = Par.main Lex.token (Lexing.from_channel ic) in
  set_prog_vars vars;
  let summary = simplify_light (analyze_path_exp body vars) in
  Logger.log_line "Program summary:";
  Logger.log_line (to_string summary);
  Logger.log_line "";
  (match assertion with
    | None -> ()
    | Some a -> 
      if check_assert summary a then
        Logger.log_line "Assertion PASSED"
      else
        Logger.log_line "Assertion FAILED\n");
  close_in ic