open Alcotest
open MerkelTree

let t = (makeBinaryMerkel [0;1;2;3;4;5;6;7;8;9;0]);;
let p = Option.get (trimProof t 1)


let testProof1 () = (check bool) "" true (prove "" p);;

let testProof2 () = (check bool) "" false (prove "" p);;



let newTest =
	run "Utils" [
		"Membership test",[
			test_case "first" `Quick testProof1;
			test_case "second" `Quick testProof2;

		];
	]
