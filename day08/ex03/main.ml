let () =
	let ethane = new Alkane.ethane in
	let ethane2 = new Alkane.ethane in
	let methane = new Alkane.methane in
	let octane = new Alkane.octane in
	print_endline ethane#to_string
	; print_endline methane#to_string
	; print_endline octane#to_string
	; print_endline (string_of_bool (ethane#equals ((ethane2 :> Alkane.alkane) :> Molecule.molecule)))
	; print_endline (string_of_bool (ethane#equals ((octane  :> Alkane.alkane) :> Molecule.molecule)))
