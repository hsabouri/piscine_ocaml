let caesar s n =
	String.map (
		fun c -> char_of_int (((int_of_char c) + n) mod 128)
	) s

let rot42 s =
	caesar s 42

let xor s k =
	let xor_key k c = char_of_int ((int_of_char c) lxor (k mod 128)) in
	String.map (xor_key k) s

let rec ft_crypt s (l:(string -> string) list) = match l with
	| f :: tail -> ft_crypt (f s) tail
	| [] -> s
