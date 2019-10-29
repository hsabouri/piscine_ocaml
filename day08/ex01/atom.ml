let get_score sym = match sym with
	| "C" -> 0
	| "H" -> 1
	| "O" -> 2
	| any -> ( String.compare "A" sym ) + 3

class virtual atom name symbol atomic_number atomic_mass = object ( self )
	method name: string = name
	method symbol: string = symbol
	method atomic_number: int = atomic_number
	method atomic_mass: float = atomic_mass

	method to_string =
		Printf.sprintf ("%s of atomic mass %f and symbol '%s' has atomic_number %d")
			self#name self#atomic_mass self#symbol self#atomic_number
	method equals (other : atom) = self#symbol = other#symbol
	method compare (other : atom) = (get_score self#symbol) - (get_score self#symbol)
end

class hydrogen = object ( self )
	inherit atom "Hydrogen" "H" 1 1.0008 as super
end

class carbon = object ( self )
	inherit atom "Carbon" "C" 6 12.01 as super
end

class oxygen = object ( self )
	inherit atom "Oxygen" "O" 8 16.0 as super
end

class uranium = object ( self )
	inherit atom "Uranium" "U" 92 238.03 as super
end

class plutonium = object ( self )
	inherit atom "Plutonium" "Pu" 94 144.0 as super
end

class lithium = object ( self )
	inherit atom "Lithium" "Li" 3 6.941 as super
end

class gold = object ( self )
	inherit atom "Gold" "Au" 79 196.97 as super
end

class einsteinium = object ( self )
	inherit atom "Einsteinium" "Es" 99 152.0 as super
end

class silicon = object ( self )
	inherit atom "Silicon" "Si" 14 28.09 as super
end

class helium = object ( self )
	inherit atom "Helium" "He" 2 4.003 as super
end

class nitrogen = object ( self )
	inherit atom "Nitrogen" "N" 8 16.0 as super
end
