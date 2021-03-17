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
let slice original_lst i j = 
  let rec slice_beginning counter k lst = 
    if counter < k then slice_beginning (counter + 1) k (List.tl lst)
    else lst
  in 
  let rec slice_end counter k lst = 
    match lst with 
    | [] -> []
    | h::t -> if counter < k then h::(slice_end (counter + 1) (k) (t)) else []
  in
  slice_end 0 (j-i) (slice_beginning 0 i original_lst);;

(* 1.5 *)
(* Partitions a list into equivalnce classes according to the equivalance function, equivalenceFunc *)
let rec equivs equivalenceFunc lst = 
  let equals = [List.hd lst]
  and 
  notEquals = []
in 
(* This helper function compares an element with every other element in lst, and puts them either into a notEquals list or an equals list. *)
let rec compare elementToCompare restOflst =
  match restOflst with
  |[] -> []
  | h::t -> if equivalenceFunc (elementToCompare) (h) = true then h::equals else h::notEquals
  @ compare (elementToCompare) (List.tl restOflst)
in
compare (List.hd lst) (List.tl lst) :: equivs (equivalenceFunc) (notEquals);;

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

(* 1.8 *)
(* Takes two parameters: a function cmp that compares two elements of a specific type T and returns one of them and a list of that same type T.
It returns a list that applies cmp while taking two elements at a time from the list. If the list has odd size, the last element is just returned "as is" *)
let rec pairwisefilter cmp lst = 
  match lst with
  | a::b::(_ as t) -> (cmp a b)::pairwisefilter cmp t
  | [] -> []
  | (_ as last)::[] -> [last];;

(* 1.9 *)
(* Takes a list of tuples and returns a polynomial function corresponding to that list. 
Each tuple in the input list consists of the coefficient and the exponent. *)
let polynomial lst = 
  let first = List.hd lst
  and second = List.hd (List.tl lst)
  and third = List.hd (List.tl (List.tl lst))
in
  let coeff1 = fst first
  and exp1 = snd first
  and coeff2 = fst second
  and exp2 = snd second
  and coeff3 = fst third 
  and exp3 = snd third
in 
fun x -> coeff1 * (pow x exp1) + coeff2 * (pow x exp2) + coeff3 * (pow x exp3);;

(* 1.10 *)
(* Takes a list and returns a list of lists that represnts the powerset of the input list. *)
let powerset lst = 
  let rec helperMap f lst = 
    match lst with
    | [] -> []
    | h::t -> (f h) :: (helperMap f t) 
  in 
  let rec to_pwr_set lst = 
    match lst with 
    | [] -> [[]]
    | h::t -> let rest = (to_pwr_set t) in rest @ (helperMap (fun (element) -> h::element) rest)
  in
  to_pwr_set lst;; 
