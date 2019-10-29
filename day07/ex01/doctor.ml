class doctor name sidekick age = object
	val _name : string = name
	val mutable _age : int = age
	val _sidekick : People.people = sidekick
	val mutable _hp = 100

	initializer print_endline ("New Doctor named " ^ _name ^ ", with hp set to " ^ string_of_int _hp ^ ", a sidekick named " ^ sidekick#get_name ^ " . I hope it will survive Dalecs..")

	method to_string = "doctor : { name: " ^ _name ^ ", age: " ^ string_of_int _age ^ " hp: " ^ string_of_int _hp ^ ", sidekick: " ^ _sidekick#to_string ^ " }"
	method talk = print_endline "Hi! I’m the Doctor!"
	method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
	method private regenerate = _hp <- 100
	method travel_in_time start arrival =
		_age <- (_age + (arrival - start))
		; print_endline "\x1b[34m
        ___
_______(_\x1b[33m@\x1b[34m_)_______
| \x1b[0mPOLICE      BOX\x1b[34m |
|_________________|
 | \x1b[0m_____\x1b[34m | \x1b[0m_____\x1b[34m |
 | \x1b[0m|###|\x1b[34m | \x1b[0m|###|\x1b[34m |
 | \x1b[0m|###|\x1b[34m | \x1b[0m|###|\x1b[34m |
 | _____ | _____ |
 | |\x1b[0m###\x1b[34m| | || || |
 | |\x1b[0m###\x1b[34m| | ||_|| |
 | _____ |$_____ |
 | || || | || || |
 | ||_|| | ||_|| |
 | _____ | _____ |
 | || || | || || |
 | ||_|| | ||_|| |
 |       |       |
 *****************\x1b[0m"
end
