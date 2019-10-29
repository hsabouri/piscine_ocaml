let read_to_string f =
	let channel = open_in f in
	let ret = really_input_string channel (in_channel_length channel) in
	close_in channel
	; ret

let () =
	Random.self_init ()
	; try
		let jokes_str = read_to_string "dad.jokes" in
		let jokes = String.split_on_char '\n' jokes_str in
		let joke = ref "" in
		for i = 0 to Random.int ((List.length jokes) - 1) do
			joke := List.nth jokes i
		done
		; print_endline !joke
	with
		| _ -> print_endline "Couldn't read joke file"
