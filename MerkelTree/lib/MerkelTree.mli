(* 
The merkelTree type.
	All vertices are represented by the same record, 
	Leafs are simply those vertices that have an empty list of children.
*)
type 'a merkelTree


(*takes a value x  and creates a leaf holding the hash of x*)
val makeLeaf : 'a -> 'a merkelTree

(*join takes a list of tree nodes and creates a new node with all of them as children*)
val join :'a  merkelTree  -> 'a merkelTree  -> 'a merkelTree

(* takes a list of values and creates a binary Merkel tree hashing those values*)
val makeBinaryMerkel : 'a list -> 'a merkelTree

(* a very rudimentary print function for testing purposes*)
val string_of_tree : 'a merkelTree -> string

(* a pretty print function which lays the tree out in a multiline setup*)
val pprint : 'a merkelTree -> string

val trimProof : 'a merkelTree -> 'a -> 'a merkelTree option

val prove : string -> 'a merkelTree -> bool

