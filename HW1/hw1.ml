(* subset function that return true if a is a subset of b, false otherwrise *)
let rec subset a b =
	match a with
	  [] -> true
	| h::t -> if List.exists (fun element -> element = h) b then subset t b else false;;

(* return true if a and b are equal sets *)
let equal_sets a b = if (subset a b) && (subset b a) then true else false;;

(* return a list of union between two sets *)
let rec set_union a b = match a with
  [] -> b
| h::t -> if List.exists (fun x -> x = h) b then set_union t b else set_union t (h::b);; 

(* return a list that contains the intersection of two sets a and b *)
let rec set_intersection a b = List.filter(fun x -> List.exists (fun y -> y = x)a)b;;

(* return a list that contains the difference between two sets *)
let set_diff a b = List.filter(fun x -> List.for_all (fun y -> y <> x)b)a;;

(* compute the fixed point of the function *)
let rec computed_fixed_point eq f x = if (eq(f x)x) then x else computed_fixed_point eq f (f x);;

(* helper function that iterate to find if fx *)
let rec iterate_to_find_x f p x = 
	if p = 1 then f x 
    else iterate_to_find_x f (p-1) (f x);;

(* computed the the periodic point of a function *)
let rec computed_periodic_point eq f p x = 
	if p = 0 then x 
    else if p = 1 then computed_fixed_point eq f x
    else if eq(iterate_to_find_x f p x) x  then x 
    else computed_periodic_point eq f p (f x);;

(* Write a function while_away s p x that returns the longest list [x; s x; s (s x); ...] such that p e is true for every element e in the list. *)
let rec while_away s p x =  
	if (p x) = false then []
    else x::(while_away s p (s x));; 

(* helper function for the rle_decode function *)
let rec pair_decode lis l r = 
	if l = 0 then lis 
    else pair_decode (r::lis) (l-1) r;; 

(*Write a function rle_decode lp that decodes a list of pairs lp in run-length encoding form.*)
let rec rle_decode lp = 
	match lp with
	| [] -> []
	| h::t -> match h with (l,r) -> pair_decode [] l r@rle_decode t;;

(* Write a function filter_blind_alleys g that returns a copy of the grammar g with all blind-alley rules removed. This function should preserve the order of rules *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let rec check_element x lis = 
	match lis with 
	[] -> false 
	| h::t -> if h = x then true else check_element x t;;

let rec check_t_symbl symbl lis_symbl = 
	match symbl with
	T _ -> true 
	| N s -> check_element s lis_symbl;;

let rec verify_rules rules lis_t_symbl =
	match rules with 
	[] -> true 
	| h::t -> if (check_t_symbl h lis_t_symbl) = false then false
        else verify_rules t lis_t_symbl;;

let rec construct_derivable_grammar g lis_t_symbl =
	match g with
	[] -> lis_t_symbl
	| (x, r)::t -> if(verify_rules r lis_t_symbl) && not (check_element x lis_t_symbl) then (construct_derivable_grammar t (x::lis_t_symbl))
	else (construct_derivable_grammar t lis_t_symbl);;


let construct_grammar_wrapper (origin_lis, lis_t_symbl) =
	origin_lis, (construct_derivable_grammar origin_lis lis_t_symbl);;

let construct_equal_set_wrapper (h1, t1) (h2, t2) = equal_sets t1 t2;;

let rec filtering_rules r lis_t_symbl new_r = 
	match r with 
	[] -> new_r 
	| (x,r)::t -> if not (verify_rules r lis_t_symbl) then filtering_rules t lis_t_symbl new_r 
	else filtering_rules t lis_t_symbl (new_r@[(x,r)]);;

let filter_blind_alleys g = 
	(fst g), (filtering_rules (snd g) (snd (computed_fixed_point construct_equal_set_wrapper construct_grammar_wrapper((snd g),[]))) []);;
