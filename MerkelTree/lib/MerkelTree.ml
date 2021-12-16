(* The merkelTree type, all vertices are represented by the same record, leafs are simply those vertices that have an empty list of children*)
type 'a merkelTree = { hash : int ; children : 'a merkelTree list;};;



let makeLeaf v = {hash = Hashtbl.hash v; children= [];};;	

let join l =  {hash = Hashtbl.hash l; children = l; };;

let rec string_of_tree t =  ((string_of_int t.hash))  ^ " with ["  ^ (String.concat "," (List.map string_of_tree t.children)) ^ "]";;

let pprint tree = 
	let rec front n = match n with
		| 0 -> "";
		| n -> "\t" ^ (front (n-1)); in
	let rec aux n t = "\n" ^ (front n) ^ (string_of_int t.hash) ^ (String.concat "" (List.map (aux (n+1)) t.children));
	in aux 0 tree;;

                                                                  
let makeBinaryMerkel l = print_endline "making"; 
	let rec pairOff l = match l with
		| fst :: snd :: rst	-> (join [fst;snd]) :: pairOff rst;
		| fst :: lst		-> (join (fst::lst))::[];
		| []			-> []; in
	let rec stackUp l = match pairOff l with
		| root :: []	-> root; 
		| l 		-> print_endline "stacking";stackUp l;
	in stackUp (List.map makeLeaf l);;

