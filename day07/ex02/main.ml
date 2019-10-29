
let () =
	let sideKick = new People.people "Amy" in
	let random_people = new People.people "Jacques Chirac" in
	let doctor = new Doctor.doctor "Doctor Who" sideKick 700 in
	let dal = new Dalek.dalek in
	random_people#talk
	; print_string "Dalek: " ; dal#talk
	; print_string "Doctor: " ; doctor#talk
	; print_string "Jacques Chirac: " ; random_people#talk
	; print_string "Dalek: Exterminates Jaques Chirac\nJacques Chirac: " ; dal#exterminate random_people
	; print_string "Doctor: " ; doctor#use_sonic_screwdriver
	; print_string "Dalek: " ; dal#talk
	; print_string "Dalek: " ; dal#die
	; print_string "Doctor: " ; doctor#travel_in_time 2006 2052
	; print_string "Dalek: " ; dal#die
	; print_string "Doctor: " ; doctor#travel_in_time 2006 2052
	; print_string "Dalek: " ; dal#die
	; print_string "Doctor: " ; doctor#travel_in_time 2006 2052
	; print_endline "And it goes for centuries..."
