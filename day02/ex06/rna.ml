(* ------------------ ex04 -------------------- *)

type phosphate = string
type deoxyribose = string
type nucleobase =
	| A
	| T
	| C
	| G
	| U
	| None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase;
}

let nucleobase_of_char c = match c with
	| 'a' | 'A' -> A
	| 't' | 'T' -> T
	| 'c' | 'C' -> C
	| 'g' | 'G' -> G
	| 'u' | 'U' -> U
	| _ -> None

let generate_nucleotide_of_nucleobase n =
	{
		phosphate = "phosphate"
		; deoxyribose = "deoxyribose"
		; nucleobase = n
	}

let generate_nucleotide c =
	generate_nucleotide_of_nucleobase (nucleobase_of_char c)

(* ------------------ ex05 -------------------- *)

type helix = nucleotide list

let get_random_nucleotide () =
	generate_nucleotide (
		match Random.int 4 with
			| 0 -> 'A'
			| 1 -> 'T'
			| 2 -> 'C'
			| 3 -> 'G'
			| _ -> 'A'
	)

let string_of_nucleobase n = match n with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| U -> "U"
	| None -> "None"

let complementary_nucleobase_of_rna n = match n with
	| A -> T
	| T -> A
	| C -> G
	| G -> C
	| U -> A
	| None -> None

let complementary_nucleotide n f =
	{
		phosphate = n.phosphate
		; deoxyribose = n.deoxyribose
		; nucleobase = (f n.nucleobase)
	}

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
	fold [] h (fun acc e -> acc @ [complementary_nucleotide e complementary_nucleobase_of_rna])

(* ------------------ ex06 -------------------- *)

type rna = nucleobase list

let complementary_nucleobase_for_rna n = match n with
	| A -> U
	| T -> A
	| C -> G
	| G -> C
	| U -> A
	| None -> None

let generate_rna (h:helix) : rna =
	fold [] h (fun acc e -> acc @ [complementary_nucleobase_for_rna e.nucleobase])

let helix_of_rna (r:rna) : helix =
	fold [] r (fun acc e -> acc @ [complementary_nucleotide (generate_nucleotide_of_nucleobase e) complementary_nucleobase_of_rna])

let string_of_rna (h:rna) =
	fold "" h (fun acc e -> acc ^ (string_of_nucleobase e))


let () =
	Random.self_init ()
	; let test n =
		let hel = generate_helix n in
		let leh = complementary_helix hel in
		print_endline ("helix < " ^ string_of_int n ^ " : " ^ helix_to_string hel)
		; print_endline ("helix" ^ " > " ^ string_of_int n ^ " : " ^ helix_to_string leh)
		; print_endline ""
	in
	let testrna n =
		let hel = generate_helix n in
		let leh = generate_rna hel in
		let hel_back = helix_of_rna leh in
		print_endline ("helix < " ^ string_of_int n ^ " : " ^ (helix_to_string hel))
		; print_endline ("rna   > " ^ string_of_int n ^ " : " ^ (string_of_rna leh))
		; print_endline ("helix > " ^ string_of_int n ^ " : " ^ (helix_to_string hel_back))
		; print_endline ""
	in
	test (-1)
	; test 0
	; test 1
	; test 10
	; test 100
	; testrna (-1)
	; testrna 0
	; testrna 1
	; testrna 10
	; testrna 100
