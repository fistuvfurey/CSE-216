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

let rec evaluate arithmetic_expression = 
    match arithmetic_expression with 
    | Var var -> 0
    | Const const -> const
    | Plus node -> evaluate node.arg1 + evaluate node.arg2
    | Minus node -> evaluate node.arg1 - evaluate node.arg2
    | Mult node -> evaluate node.arg1 * evaluate node.arg2
    | Div node -> evaluate node.arg1 / evaluate node.arg2