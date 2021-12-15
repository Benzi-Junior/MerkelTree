(* The merkelTree type, all vertices are represented by the same record, leafs are simply those vertices that have an empty list of children*)
type merkelTree

(*takes a value x  and creates a leaf holding the hash of x*)
val makeLeaf : 'a -> merkelTree

(*join takes a list of tree nodes and creates a new node with all of them as children*)
val join : merkelTree list -> merkelTree

(* takes a list of values and creates a binary Merkel tree hashing those values*)
val makeBinaryMerkel : 'a list -> merkelTree

(* a very rudimentary print function for testing purposes*)
val string_of_tree : merkelTree -> string

(* a pretty print function which lays the tree out in a multiline setup*)
val pprint : merkelTree -> string
