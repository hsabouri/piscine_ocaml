let compress l =
	let rec loop l n_list_acc curr_len curr_val =
		match l with
			| first :: tail when first = curr_val -> loop tail n_list_acc (curr_len + 1) curr_val
			| first :: tail -> loop tail (n_list_acc @ [(curr_len, curr_val)]) 1 first
			| [] -> n_list_acc @ [(curr_len, curr_val)]
	in
	match l with
		| first :: tail -> loop tail [] 1 first
		| _ -> []

let () =
	let print_pair display (i, v) =
		"(" ^ (string_of_int i) ^ ", " ^ (display v) ^ ")"
	in

	let rec print_list printer l = match l with
		| [] -> ()
		| e::l -> print_string (printer e) ; print_string ", " ; print_list printer l
	in

	let test display l =
		print_list display l
		; print_string " ==> "
		; print_list (print_pair display) (compress l)
		; print_char '\n'
	in
	test (string_of_int) [3; 3; 3; 2; 2; 7; 8; 4; 4; 4; 4; 4; 4; 8; 8; 5; 7; 8; 8]
	; test (string_of_int) [3; 3; 3; 2; 2]
	; test (fun i -> i) ["hello"; "hello"; "hella"]
	; test (fun i -> i) ["hello"]
	; test (fun i -> i) []
