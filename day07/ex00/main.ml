
let () =
	let sideKick = new People.people "Amy" in
	let k9 = new People.people "K-9" in
	print_endline sideKick#to_string
	; print_endline k9#to_string
	; sideKick#talk
	; k9#talk
