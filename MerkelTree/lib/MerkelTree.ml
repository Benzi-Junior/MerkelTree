open Cryptokit
let hashFun a  = hash_string (Hash.sha256 ()) (hash_string  (Hash.sha256 ())  (a))


type 'a merkelTree = 
	| Internal of string * 'a merkelTree * 'a merkelTree 
	| Leaf of string * 'a 

let getHash t = match t with 
	| Internal (h,_,_)	-> h
	| Leaf (h,_)		-> h

let makeLeaf v = Leaf ((hashFun v), v)


let join l r = Internal ((hashFun ((getHash l)^(getHash r))), l, r)

let rec  string_of_tree t = match t with 
	| Internal (h,l,r)	-> (h)  ^ " with (" ^ (string_of_tree l) ^ (string_of_tree r) ^ ")"
	| Leaf	(h,_)		-> (h)

let pprint tree = 
	let rec front n = match n with
		| 0 -> "";
		| n -> "\t" ^ (front (n-1)); in
	let rec aux n t = match t with 
		| Internal (h,l,r)	->"\n" ^ (front n) ^ (h) ^ (aux (n+1) l) ^ (aux (n+1) r)
		| Leaf (h,v)		->"\n" ^ (front n) ^ (h) ^ " data: " ^ v
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

(* the step datatype is a single step in Merkle proof i.e. a hash along with instruction on weather to apply it on the left or right*)
type step = 
	| L of string 
	| R of string
(* the proof datatype is a list of proof steps *)
type proof = step list

let rec string_of_proof p = match p with
	| (L h)::xs  	-> "Left  :" ^ h ^ " " ^ (string_of_proof xs) 
	| (R h)::xs  	-> "Right :" ^ h ^ " " ^ (string_of_proof xs)
	| []		-> "" 

(* this function generates the proof contained in a trimmed down Merkle tree*)
(*TODO skip the trimproof step and go straight to the proof type *)
let genProof tree value  = 
	let rec aux tree = match tree with 
		| Internal (_,Leaf (h,_),l)	-> (R h) :: aux l 
		| Internal (_,r,Leaf (h,_))	-> (L h) :: aux r
		(*this case means there are multiple instances of value in tree,just pick a branch and run with it, this could be improved to always use shortest proof*) 
		| Internal (h,_,r)	-> (R h) :: aux r
		| Leaf (_,_)		-> []
	in List.rev (aux (Option.get (trimProof tree value)))

let prove root proof value =
	let rec proofCheck h p =  match p with 
		| (R l) :: xs	-> proofCheck (hashFun (l^h)) xs
		| (L r) :: xs	-> proofCheck (hashFun (h^r)) xs
		| []		-> h
	in (proofCheck (hashFun value) proof)=root
