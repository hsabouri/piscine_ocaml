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

(* ------------------ ex07 -------------------- *)

type aminoacid =
	| Stop
	| Ala
	| Arg
	| Asn
	| Asp
	| Cys
	| Gln
	| Glu
	| Gly
	| His
	| Ile
	| Leu
	| Lys
	| Met
	| Phe
	| Pro
	| Ser
	| Thr
	| Trp
	| Tyr
	| Val

type protein = aminoacid list

let generate_base_triplets (r:rna) =
	let rec triple_fold prot l = match l with
		| nuc_a :: nuc_b :: nuc_c :: tail -> triple_fold (prot @ [(nuc_a, nuc_b, nuc_c)]) tail
		| _ -> prot
	in
	triple_fold [] r

let decode_arn (r:rna) : protein =
	let rec iter_decode prot r = match r with
		| ((U, A, A) | (U, A, G) | (U, G, A)) :: _ -> 											prot @ [Stop]
		| (U, G, G) :: tail ->																	iter_decode (prot @ [Trp]) tail
		| (A, U, G) :: tail -> 																	iter_decode (prot @ [Met]) tail
		| ((A, A, C) | (A, A, U)) :: tail ->													iter_decode (prot @ [Asn]) tail
		| ((G, A, A) | (G, A, G)) :: tail ->													iter_decode (prot @ [Glu]) tail
		| ((G, A, C) | (G, A, U)) :: tail ->													iter_decode (prot @ [Asp]) tail
		| ((U, G, C) | (U, G, U)) :: tail ->													iter_decode (prot @ [Cys]) tail
		| ((C, A, A) | (C, A, G)) :: tail ->													iter_decode (prot @ [Gln]) tail
		| ((C, A, C) | (C, A, U)) :: tail ->													iter_decode (prot @ [His]) tail
		| ((A, A, A) | (A, A, G)) :: tail ->													iter_decode (prot @ [Lys]) tail
		| ((U, U, C) | (U, U, U)) :: tail ->													iter_decode (prot @ [Phe]) tail
		| ((U, A, C) | (U, A, U)) :: tail ->													iter_decode (prot @ [Tyr]) tail
		| ((A, U, A) | (A, U, C) | (A, U, U)) :: tail ->										iter_decode (prot @ [Ile]) tail
		| ((A, C, A) | (A, C, C) | (A, C, G) | (A, C, U)) :: tail ->							iter_decode (prot @ [Thr]) tail
		| ((G, U, A) | (G, U, C) | (G, U, G) | (G, U, U)) :: tail ->							iter_decode (prot @ [Val]) tail
		| ((G, C, A) | (G, C, C) | (G, C, G) | (G, C, U)) :: tail ->							iter_decode (prot @ [Ala]) tail
		| ((G, G, A) | (G, G, C) | (G, G, G) | (G, G, U)) :: tail ->							iter_decode (prot @ [Gly]) tail
		| ((C, C, C) | (C, C, A) | (C, C, G) | (C, C, U)) :: tail ->							iter_decode (prot @ [Pro]) tail
		| ((U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C)) :: tail ->	iter_decode (prot @ [Ser]) tail
		| ((A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U)) :: tail ->	iter_decode (prot @ [Arg]) tail
		| ((C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G)) :: tail ->	iter_decode (prot @ [Leu]) tail
		| (_, _, _) :: _ | [] ->																prot
	in
	iter_decode [] (generate_base_triplets r)

let string_of_aminoacid a = match a with
	| Stop	-> "Stop"
	| Ala	-> "Alanine"
	| Arg	-> "Arginine"
	| Asn	-> "Asparagine"
	| Asp	-> "Aspartique"
	| Cys	-> "Cysteine"
	| Gln	-> "Glutamine"
	| Glu	-> "Glutamique"
	| Gly	-> "Glycine"
	| His	-> "Histidine"
	| Ile	-> "Isoleucine"
	| Leu	-> "Leucine"
	| Lys	-> "Lysine"
	| Met	-> "Methionine"
	| Phe	-> "Penylalanine"
	| Pro	-> "Proline"
	| Ser	-> "Serine"
	| Thr	-> "Threonine"
	| Trp	-> "Tryptophane"
	| Tyr	-> "Tyrosine"
	| Val	-> "Valine"

let string_of_protein p =
	fold "" p (fun acc e -> match acc with
		| "" -> (string_of_aminoacid e)
		| acc -> acc ^ ", " ^ (string_of_aminoacid e)
	)

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
	let testprot n =
		let hel = generate_helix n in
		let leh = generate_rna hel in
		let prot = decode_arn leh in
		print_endline ("helix < " ^ string_of_int n ^ " : " ^ (helix_to_string hel))
		; print_endline ("rna   > " ^ string_of_int n ^ " : " ^ (string_of_rna leh))
		; print_endline ("prot    " ^ string_of_int n ^ " : " ^ (string_of_protein prot))
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
	; testprot (-1)
	; testprot 0
	; testprot 1
	; testprot 10
	; testprot 100
