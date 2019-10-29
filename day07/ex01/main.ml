
let () =
	let sideKick = new People.people "Amy" in
	let k9 = new People.people "K-9" in
	let doctor = new Doctor.doctor "Doctor Who" sideKick 700 in
	print_endline sideKick#to_string
	; print_endline k9#to_string
	; sideKick#talk
	; k9#talk
	; print_endline (doctor#to_string)
	; doctor#talk
	; doctor#use_sonic_screwdriver
	; doctor#travel_in_time 2006 2052
	; print_endline (doctor#to_string)
