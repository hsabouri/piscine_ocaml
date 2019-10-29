let string_of_atoms (atoms: Atom.atom list)  =
	let sorted = List.sort (fun a1 a2 -> String.compare a1#symbol a2#symbol) atoms in
	let rec compress acc (n, v) atoms = match atoms with
		| head :: tail when head#equals v -> compress acc (n + 1, v) tail
		| head :: tail -> compress (acc ^ v#symbol ^ string_of_int n) (1, head) tail
		| [] -> acc ^ v#symbol ^ string_of_int n
	in match sorted with
		| head :: tail -> compress "" (1, head) tail
		| [] -> ""

class virtual molecule name formula = object ( self )
	method name: string = name
	method formula: string = string_of_atoms formula

	method to_string = Printf.sprintf "%s: %s" self#name self#formula
	method equals (other : molecule) = self#formula = other#formula
end

class trinitrotoluene = object ( self )
	inherit molecule "Trinitrotoluene"
		((	List.init 5 (fun _ -> (new Atom.hydrogen :> Atom.atom)) )
		@ (	List.init 3 (fun _ -> (new Atom.nitrogen :> Atom.atom)) )
		@ (	List.init 6 (fun _ -> (new Atom.oxygen :> Atom.atom)) )
		@ (	List.init 7 (fun _ -> (new Atom.carbon :> Atom.atom)) ))
	as super
end

class water = object ( self )
	inherit molecule "Dihydrogen oxyde"
		((	List.init 2 (fun _ -> (new Atom.hydrogen :> Atom.atom)) )
		@ (	List.init 1 (fun _ -> (new Atom.oxygen :> Atom.atom)) ))
	as super
end

class carbondioxyde = object ( self )
	inherit molecule "Carbon dioxyde"
		((	List.init 2 (fun _ -> (new Atom.oxygen :> Atom.atom)) )
		@ (	List.init 1 (fun _ -> (new Atom.carbon :> Atom.atom)) ))
	as super
end

class methamphetamine = object ( self )
	inherit molecule "Methamphetamine"
		((	List.init 15 (fun _ -> (new Atom.hydrogen :> Atom.atom)) )
		@ (	List.init 10 (fun _ -> (new Atom.carbon :> Atom.atom)) )
		@ (	List.init 1 (fun _ -> (new Atom.nitrogen :> Atom.atom)) ))
	as super
end

class cafeine = object ( self )
	inherit molecule "Cafeine"
	( (	List.init 8 (fun _ -> (new Atom.carbon :> Atom.atom)) )
		@ (	List.init 4 (fun _ -> (new Atom.nitrogen :> Atom.atom)) )
		@ (	List.init 10 (fun _ -> (new Atom.hydrogen :> Atom.atom)) )
		@ (	List.init 2 (fun _ -> (new Atom.oxygen :> Atom.atom)) ))
	as super
end
