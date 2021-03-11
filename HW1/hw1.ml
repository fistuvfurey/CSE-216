(* Aidan Furey, ID: 112622264, HW1 *)
(* 1  Recursion and Higher-order Functions *)

(* 1.1 *)
(* Takes two int parameters x and n, and returns x^n. *)
(* x = base, n = power *)
let rec pow x n =
  if n <> 0 then
    x * pow x (n - 1)
  else
    1;;

(* Takes two float paramaters, x and n, and returns x^n. *)
(* x = base, n = power *)
let rec float_pow x n =
  if n <> 0.0 then
    x *. float_pow x (n -. 1.0)
  else
    1.0;;

(* 1.2 *)
(* Removes consecutive duplicates from a list. *)
let rec compress lst = 
  match lst with
  | a::(b::_ as t) -> if a = b then compress t
  else a::compress t
  | _ -> lst;;

(* 1.3 *)
(* Takes a list and a predicate and removes all the elements that satisfy the condition expressed in the predicate. *)
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

(* 1.5 *)
(* Partitions a list into equivalnce classes according to the equivalance functino, equivalenceFunc *)
let rec equivs equivalenceFunc lst = 
  match lst with 
  | [] -> []
  | _::[] -> []
  | a::(b::_ as t) -> if equivalenceFunc a b = true then [a; b]::equivs equivalenceFunc t
  else [a]::[b]::equivs equivalenceFunc t;;

(* 1.6 *)
(* Finds two prime numbers that sum up to a given even integer a returns them as a pair in non-decreasing order. *)
let goldbachpair evenInt = 
  let rec is_prime numToCheck divisor = 
    if divisor = 1 then true
    else if numToCheck mod divisor = 0 then false else is_prime (numToCheck) (divisor - 1)
  in 
  let rec get_pair lowerInt upperInt = 
    if lowerInt + upperInt = evenInt && (is_prime (upperInt) (upperInt - 1) && is_prime (lowerInt) (lowerInt - 1)) then (lowerInt, upperInt)
    else get_pair (lowerInt - 1) (upperInt + 1)
  in get_pair (evenInt / 2) (evenInt / 2);;

(* 1.7 *)
(* Takes three inputs: two functions, f and g, and a list. It returns true if and only if the functions have identical behavior
  on every element of the list. *)
let rec equiv_on f g lst = 
   match lst with
   | [] -> true
   | h::t -> if f h <> g h then false else equiv_on f g t;;
   
