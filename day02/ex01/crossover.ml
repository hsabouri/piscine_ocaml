let crossover la lb =
	let rec is_in_list elem lb = match lb with
		| first :: tail when first = elem -> true
		| _ :: tail -> is_in_list elem tail
		| [] -> false
	in
	let rec loop la lb l_acc = match la with
		| first :: tail -> begin
			match lb with
				| lb when (is_in_list first lb) -> loop tail lb (l_acc @ [first])
				| _ :: _ -> loop tail lb l_acc
				| [] -> l_acc
			end
		| [] -> l_acc
	in
	loop la lb []

let () =
	let print_list printer l =
		let rec loop printer l = match l with
			| [] -> ()
			| e::[] -> print_string (printer e) ;
			| e::l -> print_string (printer e) ; print_string ", " ; loop printer l
		in
		print_char '['
		; loop printer l
		; print_char ']'
	in

	let test display la lb =
		print_list display la
		; print_string " <> "
		; print_list display lb
		; print_string " ==> "
		; print_list display (crossover la lb)
		; print_char '\n'
	in
	test (fun i -> "\"" ^ i ^ "\"") ["hello"; "hella"; "helli"] ["hello"; "helli"; "hellu"]
	; test (fun i -> "\"" ^ i ^ "\"") ["hellO"; "hellO"; "helli"] ["hellO"; "helli"; "hellu"]
	; test string_of_int [] []
	; test string_of_int [1] []
	; test string_of_int [] [1]
	; test string_of_int [1] [1]
	; test string_of_int [1; 1] [1]
	; test string_of_int [1] [1; 1; 1]
