(* ------------------ ex04 -------------------- *)

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


(* ------------------ ex05 -------------------- *)

type helix = nucleotide list

let get_random_nucleotide () = match Random.int 4 with
		| 0 -> generate_nucleotide 'A'
		| 1 -> generate_nucleotide 'T'
		| 2 -> generate_nucleotide 'C'
		| 3 -> generate_nucleotide 'G'
		| _ -> generate_nucleotide 'A'

let string_of_nucleobase n = match n with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| None -> "None"

let complementary_nucleotide n =
	let nuc = match n.nucleobase with
		| A -> T
		| T -> A
		| C -> G
		| G -> C
		| None -> None
	in
	{ phosphate = n.phosphate ; deoxyribose = n.deoxyribose ; nucleobase = nuc }


let rec fold acc l f = match l with
	| head :: tail -> fold (f acc head) tail f
	| [] -> acc

let rec generator acc low up f = match low with
	| low when low < up -> generator (f acc low) (low + 1) up f
	| _ -> acc

let generate_helix n : helix =
	generator [] 0 n (fun acc _ -> acc @ [get_random_nucleotide ()])

let helix_to_string (h:helix) =
	fold "" h (fun acc e -> acc ^ (string_of_nucleobase e.nucleobase))

let complementary_helix (h:helix) =
	fold [] h (fun acc e -> acc @ [complementary_nucleotide e])

let () =
	Random.self_init ()
	; let test n =
		let hel = generate_helix n in
		let leh = complementary_helix hel in
		print_endline ("helix < " ^ string_of_int n ^ " : " ^ helix_to_string hel)
		; print_endline ("helix > " ^ string_of_int n ^ " : " ^ helix_to_string leh)
		; print_endline ""
	in
	test (-1)
	; test 0
	; test 1
	; test 10
	; test 100
