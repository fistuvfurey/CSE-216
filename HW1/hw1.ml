(* Aidan Furey, ID: 112622264, HW1 *)
(* 1  Recursion and Higher-order Functions *)

(* 1.1 *)
(* Recursive function pow, which takes two int parameters x and n, and returns x^n. *)
(* x = base, n = power *)
let rec pow x n =
  if n <> 0 then
    x * pow x (n - 1)
  else
    1;;

(* Recursive function float_pow, which takes in two float paramaters, x and n, and returns x^n. *)
(* x = base, n = power *)
let rec float_pow x n =
  if n <> 0.0 then
    x *. float_pow x (n -. 1.0)
  else
    1.0;;

(* 1.2 *)
(* Recursive function compress removes consecutive duplicates from a list. *)
let rec compress lst = 
  match lst with
  | a::(b::_ as t) -> if a = b then compress t
  else a::compress t
  | _ -> lst;;

(* 1.3 *)
(* Recursive function remove_if which takes a list and a predicate and removes all the elements that satisfy the condition expressed in the predicate. *)
let rec remove_if lst predicate = 
  match lst with
  | h::t -> if predicate h then remove_if t predicate else
    h::remove_if t predicate
    | _ -> lst;;

(* 1.4 *)
(* Takes list and two indicies, i and j, and extracts a slice of the list containing the elements from the ith (inclusive)
to the jth (not inclusive) positions of the original list. *)
let slice lst i j =
  if i > j then [] else
  let rec keep n = function
    | [] -> []
    | h::t -> if n = 0 then [] else h::keep (n-1) t
in 
let rec remove n = function
  | [] -> []
  | h::t as elementsToKeep -> if n = 0 then elementsToKeep else remove (n-1) t 
in
keep (j - i) (remove i lst);;

(*
let rec equivs equivalenceFunc lst = 
  match lst with 
  | []::[] -> []::[]
  | a::(b::_ as t) -> if equivalenceFunc (a) (b) then a::b::trues 
  else trues:
  *)