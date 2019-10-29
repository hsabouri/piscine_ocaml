module type MONOID =
sig
	type element

	val zero1 : element
	val zero2 : element

	val add : element -> element -> element
	val sub : element -> element -> element
	val mul : element -> element -> element
	val div : element -> element -> element
end

module Calc =
	functor (M : MONOID) ->
	struct
		let add = M.add
		let sub = M.sub
		let mul = M.mul
		let div = M.div

		let power a b =
			let rec loop acc a b = match b with
			| b when b > 0 -> loop (M.mul acc a) a (b - 1)
			| _ -> acc
		in loop a a b

		let rec fact a = match a with
			| a when a <= M.zero2 -> M.zero2
			| a -> M.mul a (fact (M.sub a M.zero2))
	end

module INT : (MONOID with type element = int) =
struct
	type element = int

	let zero1 = 0
	let zero2 = 1
	let add = ( + )
	let sub = ( - )
	let mul = ( * )
	let div = ( / )
end

module FLOAT : (MONOID with type element = float) =
struct
	type element = float

	let zero1 = 0.0
	let zero2 = 1.0
	let add = ( +. )
	let sub = ( -. )
	let mul = ( *. )
	let div = ( /. )
end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () =
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3.0 3));
	print_endline (string_of_int (Calc_int.add 3 3));
	print_endline (string_of_float (Calc_float.add 3.0 3.0));
	print_endline (string_of_int (Calc_int.fact 3));
	print_endline (string_of_float (Calc_float.fact 3.0));
	print_endline (string_of_int (Calc_int.fact 0));
	print_endline (string_of_float (Calc_float.fact 0.0));
	print_endline (string_of_int (Calc_int.fact 100));
	print_endline (string_of_float (Calc_float.fact 100.0));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
