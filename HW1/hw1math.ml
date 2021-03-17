(* Aidan Furey, ID: 112622264 *)
(* 2 Data Types *)

(* 2.2 *)
(* Tree representing an arithmetic expression *)

type expr = 
    | Const of int
    | Var of string
    | Plus of node
    | Minus of node
    | Mult of node
    | Div of node
    and node = 
        { arg1: expr;
        arg2: expr }
