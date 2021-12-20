(* 
The merkelTree type.
	All vertices are represented by the same record, 
	Leafs are simply those vertices that have an empty list of children.
*)
type 'a merkelTree

val getHash :  'a merkelTree -> string

(*takes a value x  and creates a leaf holding the hash of x*)
val makeLeaf : string -> string merkelTree

(*join takes a list of tree nodes and creates a new node with all of them as children*)
val join :'a  merkelTree  -> 'a merkelTree  -> 'a merkelTree

(* takes a list of values and creates a binary Merkel tree hashing those values*)
val makeBinaryMerkel : string list -> string merkelTree

(* a very rudimentary print function for testing purposes*)
val string_of_tree : 'a merkelTree -> string

(* a pretty print function which lays the tree out in a multiline setup*)
val pprint : string merkelTree -> string

(* trimProof t v returns a subtree of t that includes just those vertices  that are required to prove v is a member of the set*)
val trimProof : 'a merkelTree -> 'a -> 'a merkelTree option

type proof
val string_of_proof : proof -> string
val genProof : 'a merkelTree -> 'a -> proof
(* prove r p checks if p is a valid proof for a tree with hash r in the root*)
val prove : string ->  proof -> string -> bool

