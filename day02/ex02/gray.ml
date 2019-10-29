let gray n =
	let rec count_ones ?(acc=0) l = match l with
		| elem :: tail when elem = true -> count_ones ~acc:(acc + 1) tail
		| _ :: tail -> count_ones ~acc tail
		| [] -> acc
	in

	let parity n = n mod 2 = 0
	in

	let rec generate_list ?(acc_list=[]) v n = match n with
		| n when n <= 0 -> acc_list
		| n -> generate_list ~acc_list:(v :: acc_list) v (n - 1)
	in

	let rec display l c = match l with
		| first :: tail -> print_int (if first then (1) else (0)) ; display tail c
		| _ -> print_char c
	in

	let rec invert_last l = match l with
		| last :: [] -> (if last then false else true) :: []
		| head :: tail -> head :: (invert_last tail)
		| [] -> []
	in

	let rec invert_weird ones l = match (ones, l) with
		| (1, false::true::tail) -> true :: true :: tail
		| (2, true::true::tail) -> false :: true :: tail
		| (0, head::[]) -> true :: []
		| (ones, true :: tail) -> true :: (invert_weird (ones - 1) tail)
		| (ones, false :: tail) -> false :: (invert_weird ones tail)
		| (ones,  []) -> []
	in

	let rec grey_code l =
		let ones = count_ones l
		in match (ones, l) with
			| (_, []) -> print_char '\n'
			| (1, true :: _) -> display l '\n'
			| (ones, l) when parity ones -> grey_code (display l ' ' ; invert_last l)
			| (ones, l) -> grey_code (display l ' ' ; invert_weird ones l)
	in
	grey_code (generate_list false n)

let () =
	print_string "grey 0 = " ; gray 0
	; print_string "grey -1 = " ; gray (-1)
	; print_string "grey 1 = " ; gray 1
	; print_string "grey 2 = " ; gray 2
	; print_string "grey 3 = " ; gray 3
	; print_string "grey 5 = " ; gray 5
	; print_string "grey 10 = " ; gray 10
