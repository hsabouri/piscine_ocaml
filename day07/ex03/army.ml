class ['a] army (l: 'a list) = object
	val _army: 'a list = l

	method add (elem: 'a) = elem#talk ; new army (elem :: l)
	method delete = match _army with
		| elem :: tail -> elem#die ; new army tail
		| [] -> new army _army
end
