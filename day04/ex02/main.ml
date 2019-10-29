let green = "\x1b[32m"
let reset = "\x1b[0m"

let spade_3 = Card.newCard T3 Spade
let heart_king = Card.newCard King Heart
let diamond_jack = Card.newCard Jack Diamond
let club_as = Card.newCard As Club
let heart_8 = Card.newCard T8 Heart

let deck = [spade_3; heart_king; diamond_jack; club_as; heart_8]

let () =
	Printf.printf "%s-----DECK-----%s\n" green reset;
	print_endline (List.fold_left (fun acc e -> acc ^ (Card.toString e) ^ ", ") "" deck);
	print_char '\n';
	Printf.printf "%s-----COMPARE KH 8H-----%s\n" green reset;
	print_int (Card.compare heart_king heart_8); print_char '\n';
	print_char '\n';
	Printf.printf "%s-----Max 3S JD-----%s\n" green reset;
	let max = Card.max spade_3 diamond_jack in
	print_endline (Card.toStringVerbose max);
	print_char '\n';
	Printf.printf "%s-----BEST OF THE DECK-----%s\n" green reset;
	let best = Card.best deck in
	print_endline (Card.toStringVerbose best);
	Printf.printf "%s-----BEST OF AN EMPTY DECK-----%s\n" green reset;
	let best = Card.best [] in
	print_endline (Card.toStringVerbose best)
