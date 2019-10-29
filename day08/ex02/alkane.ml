class virtual alkane n = object ( self )
	inherit Molecule.molecule
		((match n with
			| 1 -> "Meth"
			| 2 -> "Eth"
			| 3 -> "Prop"
			| 4 -> "But"
			| 5 -> "Pent"
			| 6 -> "Hex"
			| 7 -> "Hept"
			| 8 -> "Oct"
			| 9 -> "Non"
			| 10 -> "Dac"
			| 11 -> "Undec"
			| 12 -> "Dodec"
			| _ -> "Gofuckyourself")
			^ "ane")

		((	List.init n (fun _ -> (new Atom.carbon :> Atom.atom)) )
		@ (	List.init (2 * n + 2) (fun _ -> (new Atom.hydrogen :> Atom.atom)) ))
	as super
	method n: int = n

	method to_string = Printf.sprintf "%s: %s (n = %d)" super#name super#formula self#n
end

class methane = object ( self )
	inherit alkane 1 as super
end

class ethane = object ( self )
	inherit alkane 2 as super
end

class octane = object ( self )
	inherit alkane 8 as super
end
