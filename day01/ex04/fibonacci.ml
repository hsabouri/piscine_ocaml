let fibonacci n =
    let rec fib n a b = begin
        if n <= 0 then
            a
        else if n == 1 then
            b
        else
            fib (n - 1) b (a + b)
    end in
    fib n 0 1

let () =
	print_endline ("fibonacci 0 = " ^ (string_of_int (fibonacci 0)))
	; print_endline ("fibonacci 1 = " ^ (string_of_int (fibonacci 1)))
	; print_endline ("fibonacci -11 = " ^ (string_of_int (fibonacci (-11))))
	; print_endline ("fibonacci 4 = " ^ (string_of_int (fibonacci 4)))
	; print_endline ("fibonacci 8 = " ^ (string_of_int (fibonacci 8)))
	; print_endline ("fibonacci 21 = " ^ (string_of_int (fibonacci 21)))
	; print_endline ("fibonacci 50 = " ^ (string_of_int (fibonacci 50)))
