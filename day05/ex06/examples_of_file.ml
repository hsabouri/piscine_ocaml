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

let print_example (a, b) =
	for i = 0 to (Array.length a) - 1 do
		print_string (string_of_float a.(i) ^ ", ")
	done
	; print_string (b ^ "\n")

let () =
	try
		print_endline ("csv size: " ^ Sys.argv.(1))
		; let exs = examples_of_file Sys.argv.(1) in
		print_endline (string_of_int (List.length exs)) ;
		for i = 0 to (List.length exs) - 1 do
			print_example (List.nth exs i)
		done
	with
		| Invalid_argument s -> print_endline s
