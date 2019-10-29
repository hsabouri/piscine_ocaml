let read_to_string f =
	let channel = open_in f in
	let ret = really_input_string channel (in_channel_length channel) in
	close_in channel
	; ret

let examples_of_file f =
	let s = read_to_string f in
	let examples_string_list = String.split_on_char '\n' s in
	let examples_list = ref [] in
	for i = (List.length examples_string_list) - 2 downto 0 do
		examples_list := (
			let sub_strings = (String.split_on_char ',' (List.nth examples_string_list i)) in
			let example_array = Array.make (List.length sub_strings) 0.0 in
			for j = 0 to (List.length sub_strings) - 2 do
				example_array.(j) <- match float_of_string_opt (List.nth sub_strings j) with
					| Some f -> f
					| None -> 0.0
			done
			; (example_array, List.hd (List.rev sub_strings))
		) :: !examples_list
	done
	; !examples_list

let eu_dist af bf =
	let sum = ref 0.0 in
	for i = 0 to (Array.length af) - 1 do
		sum := !sum +. ((af.(i) +. bf.(i)) ** 2.0)
	done
	; sqrt(!sum)

let one_nn l r =
	let lowest_dist = ref max_float in
	let neightbor = ref 0 in
	for i = 0 to (List.length l) -1 do
		let new_dist = eu_dist (fst r) (fst (List.nth l i)) in
		if new_dist < !lowest_dist then
			lowest_dist := new_dist ; neightbor := i
	done
	; snd (List.nth l !neightbor)

let print_example (a, b) =
	for i = 0 to (Array.length a) - 1 do
		print_string (string_of_float a.(i) ^ ", ")
	done
	; print_string (b ^ "\n")

let () =
	Random.self_init ()
	; try
		print_endline ("csv: " ^ Sys.argv.(1))
		; let exs = examples_of_file Sys.argv.(1) in
		let exs_test = examples_of_file Sys.argv.(2) in
		let random_radar = List.nth exs_test (Random.int (List.length exs_test)) in
		print_example random_radar
		; let result = one_nn exs random_radar in
		print_endline "\n\n Found nearest radar type :"
		; print_endline result
	with
		| Invalid_argument s -> print_endline s
