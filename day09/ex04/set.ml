module Set =
struct
	type 'a t = 'a list

	let zero = []

	let return elem = [ elem ]

	let bind t f = List.fold_left (fun acc x -> acc @ (f x)) [] t

	let union t1 t2 = t1 @ t2

	let filter t f = List.filter f t

	let inter t1 t2 = bind t1 (
		fun x -> filter t2 (( = ) x)
	)

	let foreach t f = List.iter f t

	let for_all t f = List.for_all f t

	let exists t f = List.exists f t

	let diff t1 t2 =
		let diff_one t1 t2 = bind t1 (
			fun x -> match exists t2 (( = ) x) with
				| true -> zero
				| false -> return x
		) in
		(diff_one t1 t2) @ (diff_one t2 t1)
end

let print_int_better x =
	print_string (string_of_int x ^ ", ")

let () =
	let mdr = Set.return "Hello " in
	let mdr2 = Set.return "world!" in
	let mdr3 = Set.union mdr mdr2 in
	print_string "Set.return \"Hello \" = [ " ; Set.foreach mdr (fun x -> print_string (x ^ ", ")) ; print_endline "]"
	; print_string "Set.return \"world!\" = [ " ; Set.foreach mdr2 (fun x -> print_string (x ^ ", ")) ; print_endline "]"
	; print_string "Set.union mdr mdr2 = [ " ; Set.foreach mdr3 (fun x -> print_string (x ^ ", ")) ; print_endline "]"
	; let mdr = [1; 2; 3; 4; 5] in
	let mdr2 = [4; 5; 6; 7 ; 8] in
	let mdr_n_mdr2 = Set.inter mdr mdr2 in
	let mdr_x_mdr2 = Set.diff mdr mdr2 in
	print_string "[1, 2, 3, 4, 5] N [4, 5, 6, 7, 8] = [ " ; Set.foreach mdr_n_mdr2 print_int_better ; print_endline "]"
	; print_string "[1, 2, 3, 4, 5] X [4, 5, 6, 7, 8] = [ " ; Set.foreach mdr_x_mdr2 print_int_better ; print_endline "]"
	; let mdr_is_more_zero = Set.for_all mdr_x_mdr2 ((<) 0) in
	let mdr_is_less_zero = Set.for_all mdr_x_mdr2 ((>) 0) in
	print_endline ("Set.for_all [1, 2, 3, 4, 5] N [4, 5, 6, 7, 8] < 0 = [" ^ string_of_bool mdr_is_more_zero ^ "]")
	; print_endline ("Set.for_all [1, 2, 3, 4, 5] N [4, 5, 6, 7, 8] < 0 = [" ^ string_of_bool mdr_is_less_zero ^ "]")
