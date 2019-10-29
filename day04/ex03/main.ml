let string_of_list l = List.fold_left (fun a x -> a ^ ", " ^ x) "" l

let () =
	Random.self_init ()
	; let deck = Deck.newDeck () in
	let (draw, new_deck) = Deck.drawCard deck in
	print_endline ("toStringList newDeck = " ^ string_of_list (Deck.toStringList (Deck.newDeck ())))
	; print_endline ("\ntoStringList newDeck = " ^ string_of_list (Deck.toStringList (Deck.newDeck ())))
	; print_endline ("\ntoStringList DECK = " ^ string_of_list (Deck.toStringList deck))
	; print_endline ("\ntoStringListVerbose drawDeck = " ^ string_of_list (Deck.toStringListVerbose new_deck))
	; print_endline ("\nDisplay drawn card = " ^ Deck.Card.toStringVerbose draw)
	; let (a,b) = Deck.drawCard Deck.emptyDeck in

	print_endline ("unreachable")
	(* ; print_endline ("toStringListVerbose deck = " ^ string_of_list (Deck.toStringListVerbose deck)) *)
