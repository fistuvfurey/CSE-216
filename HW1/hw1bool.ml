(* Aidan Furey, ID: 112622264, HW1 *)
(* 1  Data Types *)

(* 2.1 *)
(* Let us define a language for expressions in Boolean logic: *)
type bool_expr =
    | Lit of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr
let rec evaluate a val_a b val_b = function
    | Lit literal -> if literal = a then val_a else val_b
    | Not expression -> not(evaluate a val_a b val_b expression)
    | And(expression1, expression2) -> evaluate a val_a b val_b expression1 && evaluate a val_a b val_b expression2
    | Or(expression1, expression2) -> evaluate a val_a b val_b expression1 || evaluate a val_a b val_b expression2
let truth_table a b expression =
  [(true,  true,  evaluate a true  b true  expression);
   (true,  false, evaluate a true  b false expression);
   (false, true,  evaluate a false b true  expression);
   (false, false, evaluate a false b false expression);];;

  