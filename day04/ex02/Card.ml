module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade ; Heart ; Diamond ; Club]

	let toStringVerbose c = match c with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"

	let toString c = match c with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"
end

module Value =
struct
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
end

type t = Value.t * Color.t

let newCard v c = (v, c)

let private_list_of_values_and_color l c =
	let rec loop l c acc = match l with
		| head :: tail -> loop tail c (List.append acc [newCard head c])
		| [] -> acc
	in loop l c []

let allSpades =
	private_list_of_values_and_color Value.all Color.Spade

let allHearts =
	private_list_of_values_and_color Value.all Color.Heart

let allDiamonds =
	private_list_of_values_and_color Value.all Color.Diamond

let allClubs =
	private_list_of_values_and_color Value.all Color.Club

let all =
	let spades = allSpades in
	let hearts = allHearts in
	let diamonds = allDiamonds in
	let clubs = allClubs in
	List.append spades (
	List.append hearts (
	List.append diamonds clubs))

let getValue (value, _) = value

let getColor (_, color) = color

let toString (value, color) = Value.toString value ^ Color.toString color

let toStringVerbose (value, color) =
	Printf.sprintf "Card (%s, %s)" (Value.toStringVerbose value) (Color.toStringVerbose color)

let compare (x, _) (y, _) = (Value.toInt x) - (Value.toInt y)

let max card_a card_b = match compare card_a card_b with
	| diff when diff < 0 -> card_b
	| _ -> card_a

let min card_a card_b = match compare card_a card_b with
	| diff when diff <= 0 -> card_a
	| _ -> card_b

let best l = match l with
	| head :: tail -> List.fold_left max head tail
	| [] -> invalid_arg "Empty list of Cards"

let isOf (_, c_c) c = c_c = c

let isSpade card = isOf card Color.Spade
let isHeart card = isOf card Color.Heart
let isDiamond card = isOf card Color.Diamond
let isClub card = isOf card Color.Club
