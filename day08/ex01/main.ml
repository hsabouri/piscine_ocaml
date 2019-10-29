let () =
	let meth = new Molecule.methamphetamine in
	let meth2 = new Molecule.methamphetamine in
	let coffee = new Molecule.cafeine in
	print_endline meth#to_string
	; print_endline coffee#to_string
	; print_endline (string_of_bool (meth#equals meth2))
	; print_endline (string_of_bool (meth#equals coffee))
