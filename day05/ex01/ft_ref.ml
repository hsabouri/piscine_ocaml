type 'a ft_ref = { mutable contents : 'a }

let return a = { contents = a }

let get r = r.contents

let set r n = r.contents <- n

let bind r f = f (get r)

let () =
	let x = 42 in
	let r_x = return x in
	let get_x1 = get r_x in
	set r_x 21
	; print_endline ("let x = " ^ string_of_int x)
	; print_endline ("let r_x return x -> ()")
	; print_endline ("get r_x = " ^ string_of_int(get_x1))
	; print_endline ("set r_x 21 -> () ")
	; print_endline ("get r_x = " ^ string_of_int(get r_x))
	; print_endline ("get (bind r_x (fun a -> return (a * 2))) = " ^ string_of_int (get (bind r_x (fun a -> return (a * 2)))))
	; print_endline ("get (bind r_x (fun a -> return (string_of_int a))) = " ^ (get (bind r_x (fun a -> return (string_of_int a)))))
