let converges f x n =
    if n < 0 then
        false
    else begin
        let rec iter f x n =
            if n == 0 then
                x
            else if n == 1 then
                f x
            else
                f (iter f x (n - 1))
        in
        let a = iter f x n in
        let b = iter f x (n + 1) in
        a == b
    end

let () =
	print_endline ("converges (fun x -> x * x) 2 4 = " ^ (string_of_bool (converges (fun x -> x * x) 2 4)))
	; print_endline ("converges (fun x -> x * 2) 2 4 = " ^ (string_of_bool (converges (fun x -> x * 2) 2 4)))
	; print_endline ("converges (fun x -> x) 2 1000 = " ^ (string_of_bool (converges (fun x -> x) 2 1000)))
	; print_endline ("converges (fun x -> x + 1) 0 1000 = " ^ (string_of_bool (converges (fun x -> x + 1) 0 1000)))
	; print_endline ("converges (fun x -> x - 1) 1000 1000 = " ^ (string_of_bool (converges (fun x -> x - 1) 1000 1000)))
	; print_endline ("converges (fun x -> x) 0 (-1) = " ^ (string_of_bool (converges (fun x -> x) 0 (-1))))
	; print_endline ("converges (( * ) 2) 2 5 = " ^ (string_of_bool (converges (( * ) 2) 2 5)))
	; print_endline ("converges (fun x -> x / 2) 2 3 = " ^ (string_of_bool (converges (fun x -> x / 2) 2 3)))
	; print_endline ("converges (fun x -> x / 2) 2 2 = " ^ (string_of_bool (converges (fun x -> x / 2) 2 2)))
