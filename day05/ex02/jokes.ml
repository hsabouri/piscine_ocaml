let () =
	Random.self_init ()
	; let a = [|
		"I wanted to tell a time traveling joke but you didn't like it."
		; "How do you make holy water? You boil the hell out of it."
		; "Did you know the first French fries weren't actually cooked in France? They were cooked in Greece."
		; "What do you call someone with no body and no nose? Nobody knows."
		; "I ordered a chicken and an egg from Amazon. I’ll let you know." |] in
	let joke = Array.get a (Random.int 5) in
	print_endline joke
