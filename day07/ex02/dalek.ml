let rec random_chars n = match n with
	| n when n > 0 -> String.make 1 (char_of_int ((Random.int 26) + int_of_char 'a')) ^ random_chars (n - 1)
	| _ -> ""

class dalek = object
	val _name = "Dalek" ^ random_chars 3
	val _hp = 100
	val mutable _shield = true

	method exterminate (p: People.people) = _shield <- (_shield != true) ; p#die
	method to_string = "people : { name: " ^ _name ^ ", hp: " ^ string_of_int _hp ^ ", shield: " ^ string_of_bool _shield ^ " }"
	method talk =
		let quotes = ["Explain! Explain!" ; "Exterminate! Exterminate!" ; "I obey!" ; "You are the Doctor! You are the enemy of the Daleks!"] in
		let quote = List.nth quotes (Random.int ((List.length quotes) - 1)) in
		print_endline quote

	method die = print_endline "Emergency Temporal Shift!"
end
