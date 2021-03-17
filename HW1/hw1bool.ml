(* Aidan Furey, ID: 112622264, HW1 *)
(* 2  Data Types *)

(* 2.1 *)
(* Let us define a language for expressions in Boolean logic: *)
type bool_expr =
    | Lit of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr 

let rec eval_expr a a_val b b_val expr = 
    match expr with 
    | Lit lit -> if lit = a then a_val else b_val
    | Not not_expr -> not(eval_expr a a_val b b_val not_expr)
    | And (and_expr1, and_expr2) -> eval_expr a a_val b b_val and_expr1 && eval_expr a a_val b b_val and_expr2
    | Or (or_expr1, or_expr2) -> eval_expr a a_val b b_val or_expr1 || eval_expr a a_val b b_val or_expr2
let truth_table a b bool_expr =  
    [(true, true, eval_expr a true b true bool_expr);
    (true, false, eval_expr a true b false bool_expr);
    (false, true, eval_expr a false b true bool_expr);
    (false, false, eval_expr a false b false bool_expr)]



