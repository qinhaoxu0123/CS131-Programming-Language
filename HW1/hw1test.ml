(* subset *)
let subset_test0 = not (subset [1;2;3;4] [4])
let subset_test1 = subset [] []
let subset_test2 = subset [4] [3;4;5]
let subset_test3 = subset [1] [1]
let subset_test4 = not(subset [1] [])

(* equal_sets *)
let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;1;2] [1;2]
let equal_sets_test2 = equal_sets [1] [1]
let equal_sets_test3 = not (equal_sets [] [1])
let euqal_sets_test4 = not (equal_sets [1] [])

(* set_union *)
let set_union_test0 = equal_sets (set_union [1;2] []) [1;2]
let set_union_test1 = equal_sets (set_union [1;1] [2;2]) [1;2]

(* set_intersection *)
let set_intersection_test0 =
  equal_sets (set_intersection [1;2;3] [2;4]) [2]
let set_intersection_test1 =
  equal_sets (set_intersection [1] [2;4]) []
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;4] [2;4]) [2;4]

(* set_diff *)
let set_diff_test0 = equal_sets (set_diff [] []) []
let set_diff_test1 = equal_sets (set_diff [1;2;3;4;2] [4;1;4]) [2;3]

(* computed_fixed_point *)
let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25)
(* computed_periodic_point *)
let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 2) 0 (-1) = -1
let computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5 = -1.

(* while_away *)
let while_away_test0 = 
  equal_sets (while_away ((+) 1) ((>) 4) 0) [0; 1; 2; 3]
let while_away_test1 =
  not(equal_sets (while_away ((+) 2) ((>) 10) 1) [1; 3; 5; 7])

(* rle_decode *)
let rle_decode_test0 = 
  equal_sets (rle_decode [2,0; 1,6;4,2]) [0; 0; 6; 2; 2; 2; 2]
let rle_decode_test1 =
  equal_sets (rle_decode [3,"w"; 1,"x"; 0,"y"; 0,"z"]) ["w"; "w"; "w"; "x"]
let rle_decode_test2 = 
  equal_sets (rle_decode [0,0; 0,0; 0, 0]) []



(* An example grammar for a small subset of Awk.  *)

type nonterminals = | A | B | C | D 

let grammar = [A, [T "a"]; 
	       A, [T "c"; N B];
	       B, [T "d"];
	       C, [N D; T "b"];
               D, [N C; T "a"]]
let grammar_rules = A, grammar

let filter_blind_alleys_test0 = filter_blind_alleys grammar_rules =
     (A, [A, [T "a"];
	A, [T "c"; N B];
	B, [T "d"]])







