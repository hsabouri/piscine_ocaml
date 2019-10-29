let sum a b = (a) +. (b)

let () =
	let a = sum 10.9 9.1 in
	let b = sum 10.9 (-10.9) in
	let c = sum a a in
	print_endline ("let a = sum 10.9 9.1 = " ^ string_of_float a)
	; print_endline ("let b = sum 10.9 10.9 = " ^ string_of_float b)
	; print_endline ("sum a a = " ^ string_of_float c)
