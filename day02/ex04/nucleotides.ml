type phosphate = string
type deoxyribose = string
type nucleobase =
	| A
	| T
	| C
	| G
	| None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase;
}

let generate_nucleotide c =
	let nuc = match c with
		| 'a' | 'A' -> A
		| 't' | 'T' -> T
		| 'c' | 'C' -> C
		| 'g' | 'G' -> G
		| _ -> None
	in
	{ phosphate = "phosphate" ; deoxyribose = "deoxyribose" ; nucleobase = nuc}


let () =
	let display_nucleobase n = match n with
		| A -> "A"
		| T -> "T"
		| C -> "C"
		| G -> "G"
		| None -> "None"
	in
	let test c =
		let n = generate_nucleotide c
		in
		print_endline ("nucleotide '" ^ (Char.escaped c) ^ "' : phosphate = " ^ n.phosphate ^ " | deoxyribose = " ^ n.deoxyribose ^ " | nucleobase = " ^ (display_nucleobase n.nucleobase))
	in
	test 'a'
	; test 't'
	; test 'c'
	; test 'g'
	; test 'A'
	; test 'T'
	; test 'C'
	; test 'G'
	; test 'u'
