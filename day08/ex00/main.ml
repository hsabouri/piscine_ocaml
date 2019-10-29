let () =
	let hydrogen = new Atom.hydrogen in
	let oxygen = new Atom.oxygen in
	let carbon = new Atom.carbon in
	print_endline hydrogen#to_string
	; print_endline oxygen#to_string
	; print_endline carbon#to_string
