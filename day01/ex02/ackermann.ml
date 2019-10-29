let rec ackermann m n =
    if m == 0 then
        n + 1
    else if m > 0 && n == 0 then
        ackermann (m - 1) 1
    else if m > 0 && n > 0 then
        ackermann (m - 1) (ackermann m (n - 1))
    else
        -1

let () =
	print_endline ("ackermann -1 7 = " ^ string_of_int (ackermann (-1) 7))
    ; print_endline ("ackermann 7 -1 = " ^ string_of_int (ackermann 7 (-1)))
    ; print_endline ("ackermann 0 0 = " ^ string_of_int (ackermann 0 0))
    ; print_endline ("ackermann 2 3 = " ^ string_of_int (ackermann 2 3))
    ; print_endline ("ackermann 4 1 = " ^ string_of_int (ackermann 4 1))
