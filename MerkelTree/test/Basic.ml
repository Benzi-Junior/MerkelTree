open MerkelTree

let t = (makeBinaryMerkel [0;1;2;3;4;5;6;7;8;9;0]);;
let p = Option.get (trimProof t 1)




let () =
	print_endline (string_of_int (Hashtbl.hash 0));
	print_endline (string_of_tree (makeLeaf 0));
	print_endline (string_of_int (Hashtbl.hash 1));
	print_endline (string_of_tree (makeLeaf 1));
	print_endline (string_of_tree (join (makeLeaf 0) (makeLeaf 1)));
	print_endline (pprint t);
	print_endline (pprint (Option.get (trimProof t 1)));

