
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let rec rules_converter rules lis1 x = 
	match rules with
	| [] -> lis1
	| (r_head::r_tail) -> match r_head with
		| (left_expr,right_expr) -> if(left_expr=x) then rules_converter r_tail (lis1@[right_expr]) x
		else rules_converter r_tail lis1 x


let convert_grammar gram1 = 
	match gram1 with 
	| (start_symbl, rules) -> (start_symbl, (rules_converter rules []))


(*
Write a function parse_prefix gram that returns a matcher for the grammar gram. When applied to an acceptor accept and a fragment frag, the matcher must return the first acceptable match of a prefix of frag, by trying the grammar rules in order; this is not necessarily the shortest nor the longest acceptable match. A match is considered to be acceptable if accept succeeds when given a derivation and the suffix fragment that immediately follows the matching prefix. When this happens, the matcher returns whatever the acceptor returned. If no acceptable match is found, the matcher returns None.
*)
let parse_prefix grammar acceptor fragment = 
(*this dfs_matcher will inspect the fst_rule from the bt_matcher's select_rules and it will recursively call if the T symbol did not match with the T symbol in our fragment*)
	let rec dfs_matcher origin_rules select_rule acceptor derivation fragment =  match select_rule with 
		| [] -> acceptor derivation fragment (*if the select rule is empty then we just return whatever the acceptor accepted with the derivation list and the fragment*)
		(*otherwise if the case is Nonterminal then we update our acceptor with the orgin_rules, and passing other and acceptor*)
		| ((N fst_symbl)::other) -> let update_acceptor = dfs_matcher origin_rules other acceptor 
				in (*in this situation, we will call the matcher with update_acceptor and the nonterminal symbol, for example expr -> [N Term; N Binop; N Expr] will pass in [N Bino; ......]*)
				bt_matcher fst_symbl origin_rules (origin_rules fst_symbl) update_acceptor derivation fragment 
		(*otherwise if select rule has the terminal symbol and it also match with terminal symbol in our fragment then we recursively call dfs_matcher with the rest of the fragment terminal symbls otherwise reuturn none or if our fragment is empty return empty*)
		| ((T fst_symbl)::other) -> match fragment with 
						| [] -> None
						| (h::t) -> if (h=fst_symbl) then dfs_matcher origin_rules other acceptor derivation t
							    else None 
        (*this matcher will recursively call if the dfs_matcher return None*)
	and bt_matcher start_symbl origin_rules select_rule acceptor derivation fragment =  match select_rule with 
	| [] -> None (*if the select rules are empty then we just return None*)
	(*otherwise we will call the dfs_match to inspect our the fst_rule in our selected rules and also update the dervice my append the start with fst_rule*)
	| (fst_stmt::other_stmt) -> let match_suffix = dfs_matcher origin_rules fst_stmt acceptor (derivation@[(start_symbl,fst_stmt)]) fragment 
	in 
	(*if the match_suffix return none that means that we the T terminal symbol is not in our fragment in this case we recursively call bt_matcher*)
	match match_suffix with 
			| None -> bt_matcher start_symbl origin_rules other_stmt acceptor derivation fragment
			| other -> other (*if the match suffix return anything other than None then we we accept list of derivation and move on*)
	in (*This will start the call by passing the fst and second rules and the select rule along with the acceptor, empty derivation list and fragment*)  
        bt_matcher (fst grammar) (snd grammar) ( (snd grammar) (fst grammar) ) acceptor [] fragment 












