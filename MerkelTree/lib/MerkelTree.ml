
(*I alias the hash function and the hash in order to replace them with a better function later*)
type hash = int
let hashFun = Hashtbl.hash

type 'a merkelTree = 
	| Internal of hash * 'a merkelTree * 'a merkelTree 
	| Leaf of hash * 'a 

let getHash t = match t with 
	| Internal (h,_,_)	-> h
	| Leaf (h,_)		-> h

let makeLeaf v = Leaf ((hashFun v), v)

let join l r = Internal ((hashFun ((getHash l)+(getHash r))), l, r)



let rec  string_of_tree t = match t with 
	| Internal (h,l,r)	-> (string_of_int h)  ^ " with (" ^ (string_of_tree l) ^ (string_of_tree r) ^ ")"
	| Leaf	(h,_)		-> (string_of_int h)

let pprint tree = 
	let rec front n = match n with
		| 0 -> "";
		| n -> "\t" ^ (front (n-1)); in
	let rec aux n t = match t with 
		| Internal (h,l,r)	->"\n" ^ (front n) ^ (string_of_int h) ^ (aux (n+1) l) ^ (aux (n+1) r)
		| Leaf (h,_)		->"\n" ^ (front n) ^ (string_of_int h) ^ " DataBlock"
	in aux 0 tree;;
                                                                  

let makeBinaryMerkel l = print_endline "making"; 
	let rec pairOff l = match l with
		| fst :: snd :: rst	-> (join fst snd) :: pairOff rst;
		| fst :: []		-> (fst)::[];
		| []			-> []; in
	let rec stackUp l = match pairOff l with
		| root :: []	-> root; 
		| l 		-> stackUp l;
	in stackUp (List.map makeLeaf l);;



