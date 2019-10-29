let uncaesar s n =
	String.map (
		fun c -> char_of_int (((int_of_char c) + (128 - (n mod 128))) mod 128)
	) s

let unrot42 s =
	uncaesar s 42

let ft_uncrypt s (l:(string -> string) list) =
	let rec invert acc l = match l with
		| head :: tail -> invert (head :: acc) tail
		| [] -> acc
	in
	let rec uncrypt s l = match l with
		| f :: tail -> uncrypt (f s) tail
		| [] -> s
	in
	uncrypt s (invert [] l)
