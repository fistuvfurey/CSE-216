(* Aidan Furey, ID: 112622264 *)
(* 2 Data Types *)

(* 2.2 *)
(* Tree representing an arithmetic expression *)
type expr =
    | Const of int
    | Var of string
    | Plus of expr * expr
    | Mult of expr * expr
    | Minus of expr * expr
    | Div of expr * expr
    