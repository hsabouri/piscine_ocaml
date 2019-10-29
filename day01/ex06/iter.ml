let rec iter f x n =
    if n < 0 then
        (-1)
    else if n == 0 then
        x
    else if n == 1 then
        f x
    else
        f (iter f x (n - 1))

let () =
	print_endline ("iter (fun x -> x * x) 2 4 = " ^ (string_of_int (iter (fun x -> x * x) 2 4)))
	; print_endline ("iter (fun x -> x * 2) 2 4 = " ^ (string_of_int (iter (fun x -> x * 2) 2 4)))
	; print_endline ("iter (fun x -> x) 2 1000 = " ^ (string_of_int (iter (fun x -> x) 2 1000)))
	; print_endline ("iter (fun x -> x + 1) 0 1000 = " ^ (string_of_int (iter (fun x -> x + 1) 0 1000)))
	; print_endline ("iter (fun x -> x - 1) 1000 1000 = " ^ (string_of_int (iter (fun x -> x - 1) 1000 1000)))
	; print_endline ("iter (fun x -> x) 0 (-1) = " ^ (string_of_int (iter (fun x -> x) 0 (-1))))
