open MerkelTree



let () =
	print_endline ("testchange");
	print_endline (string_of_int (Hashtbl.hash 0));
	print_endline (string_of_tree (makeLeaf 0));
	print_endline (string_of_int (Hashtbl.hash 1));
	print_endline (string_of_tree (makeLeaf 1));
	print_endline (string_of_tree (join [(makeLeaf 0);(makeLeaf 1)]));
	print_endline (pprint (makeBinaryMerkel [0;1;2;3;4;5;6;7;8;9;0]));
