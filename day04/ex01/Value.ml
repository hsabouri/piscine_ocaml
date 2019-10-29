type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

let toInt = function
	| T2 -> 1
	| T3 -> 2
	| T4 -> 3
	| T5 -> 4
	| T6 -> 5
	| T7 -> 6
	| T8 -> 7
	| T9 -> 8
	| T10 -> 9
	| Jack -> 10
	| Queen -> 11
	| King -> 12
	| As -> 13

let toStringVerbose = function
	| T2    -> "2"
	| T3    -> "3"
	| T4    -> "4"
	| T5    -> "5"
	| T6    -> "6"
	| T7    -> "7"
	| T8    -> "8"
	| T9    -> "9"
	| T10   -> "10"
	| Jack  -> "Jack"
	| Queen	-> "Queen"
	| King  -> "King"
	| As    -> "As"

let toString = function
	| T2    -> "2"
	| T3    -> "3"
	| T4    -> "4"
	| T5    -> "5"
	| T6    -> "6"
	| T7    -> "7"
	| T8    -> "8"
	| T9    -> "9"
	| T10   -> "10"
	| Jack  -> "J"
	| Queen	-> "Q"
	| King  -> "K"
	| As    -> "A"

let next t =
	let rec loop t list = match list with
		| head :: [] -> invalid_arg "As has no next value"
		| head :: next :: tail when head = t -> next
		| head :: tail -> loop t tail
		| [] -> invalid_arg "Invalid argument, I wonder how you fooled the compiler that way..."
	in loop t all

let previous t =
	let rec loop t list = match list with
		| head :: next :: tail when next = t -> head
		| head :: tail -> loop t tail
		| [] -> invalid_arg "T2 has no previous value"
	in loop t all
