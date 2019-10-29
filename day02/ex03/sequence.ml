let sequence n =
	let rec step l n_list_acc curr_len curr_val =
		match l with
			| first :: tail when first = curr_val -> step tail n_list_acc (curr_len + 1) curr_val
			| first :: tail -> step tail (n_list_acc @ [curr_len ; curr_val]) 1 first
			| [] -> n_list_acc @ [curr_len; curr_val]
	in

	let rec string_of_step str l = match l with
		| tail :: [] -> str ^ (string_of_int tail)
		| head :: tail -> string_of_step (str ^ (string_of_int head)) tail
		| [] -> str
	in

	let rec get_n l n = match (n, l) with
		| (n, 1::[]) when n < 0 -> ""
		| (n, _) when n <= 0 -> string_of_step "" l
		| (n, first :: _) -> get_n (step l [] 0 first) (n - 1)
		| (_, _) -> ""
	in

	get_n [1] n

let () =
	let rec loop acc n = match acc with
		| acc when acc >= n -> ()
		| acc -> print_endline ("sequence " ^ (string_of_int acc) ^ " = " ^ (sequence acc)) ; loop (acc + 1) n
	in
	loop (-1) 15
