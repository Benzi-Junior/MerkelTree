
(*I alias the hash function and the hash in order to replace them with a better function later*)
(*TODO replace with a better hash function *)
type hash = int
let toHash t = t
let hashFun = Hashtbl.hash

type 'a merkelTree = 
	| Internal of hash * 'a merkelTree * 'a merkelTree 
	| Leaf of hash * 'a 

let getHash t = match t with 
	| Internal (h,_,_)	-> h
	| Leaf (h,_)		-> h

let makeLeaf v = Leaf ((hashFun v), v)
(*here I am making an assumption that the hash will be an algebraic type, and if that algebra is commutative this is a significant weakening of the proof strength*)
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
                                                                  

let makeBinaryMerkel l = 
	let rec pairOff l = match l with
		| fst :: snd :: rst	-> (join fst snd) :: pairOff rst;
		| fst :: []		-> (fst)::[];
		| []			-> []; in
	let rec stackUp l = match pairOff l with
		| root :: []	-> root; 
		| l 		-> stackUp l;
	in stackUp (List.map makeLeaf l);;

(*TODO put proofs into their own datatype to simplify their structure  and reverse their order (so confirming the proof works towards root)*)

(*attempts to generate a proof for a given value being in the set that created the tree
The proof is represented by a subtree of those vertices that are *)
let rec  trimProof tree value = match tree with
	| Internal (h,l,r)	-> (match ((trimProof l value), (trimProof r value)) with
		| (None,None)	-> None
		| (None,Some t)	-> Some (Internal (h,Leaf (getHash l,value),t))
		| (Some t,None)	-> Some (Internal (h,t,Leaf (getHash r,value)))
		| (_,_)		-> Some tree)
	| Leaf (_,v)	-> (match v with 
		| v when v=value -> Some (tree) 
		| _	-> None)



(*takes a tree and a proof and checks that proof is valid for that tree
The proof is run backwards, first confirming that the root is the same and then going down the tree*)
let rec prove root proof = match proof with 
	| Internal (h,l,r)	-> (match (hashFun((getHash l)+(getHash r))=h)&&(h=root) with
		| false	-> false
		| true	-> prove (getHash l) l || prove (getHash r) r)
	| Leaf (h,v)		-> hashFun v = h
