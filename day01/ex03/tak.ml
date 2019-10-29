let rec tak x y z =
    if y < x then
        tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
    else
        z

let () =
	print_endline ("tak 3 2 3 = " ^ (string_of_int (tak 3 2 3)))
	; print_endline ("tak 5 23 7 = " ^ (string_of_int (tak 5 23 7)))
	; print_endline ("tak 0 0 0 = " ^ (string_of_int (tak 0 0 0)))
	; print_endline ("tak 1 1 1 = " ^ (string_of_int (tak 1 1 1)))
	; print_endline ("tak (-2) (-1) (-10) = " ^ (string_of_int (tak (-1) (-1) (-1))))
	; print_endline ("tak 23498 98734 98776 = " ^ (string_of_int (tak 23498 98734 98776)))
