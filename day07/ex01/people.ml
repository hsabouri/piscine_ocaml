class people _name =
	object
		val name : string = _name
		val hp = 100

		initializer print_endline ("People named " ^ name ^ ", with hp set to " ^ string_of_int hp ^ ". I hope it's not a future Cyberman.")

		method get_name = name
		method to_string = "people : { name: " ^ name ^ ", hp: " ^ string_of_int hp ^ " }"
		method talk = print_endline ("I'm " ^ name ^ "! Do you know the Doctor?")
		method die = print_endline "Aaaarghh!"

	end
