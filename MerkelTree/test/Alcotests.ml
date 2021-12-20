open Alcotest
open MerkelTree

let t = makeBinaryMerkel (List.map string_of_int [0;1;2;3;4;5;6;7;8;9;0]);;
(* Option.get is fatal when called on a None so this needs a better implementation*)
let tp = Option.get (trimProof t "1")
let p = genProof t "1"

let testTrimmer () = (check bool) "" true ((Option.get (trimProof tp "1"))=tp)

let testProof1 () = (check bool) "" true (prove (getHash t) p "1");;

let testProof2 () = (check bool) "" false (prove (getHash t) p "0");;



let newTest =
	run "Utils" [
		"Membership test",[
			test_case "first" `Quick testProof1;
			test_case "second" `Quick testProof2;

		];
		"Trimming test", [test_case "first" `Quick testTrimmer;];
	]
