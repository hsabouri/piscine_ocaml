let repeat_x n =
    if n < 0 then
        "Error"
    else
        let rec loop n s = begin
            if n > 0 then
                loop (n - 1) (s ^ "x")
            else
                s
        end in
        loop n ""

let () =
    print_endline ("repeat_x 10 = " ^ (repeat_x 10))
    ; print_endline ("repeat_x 4 = " ^ (repeat_x 4))
    ; print_endline ("repeat_x 1 = " ^ (repeat_x 1))
    ; print_endline ("repeat_x 0 = " ^ (repeat_x 0))
    ; print_endline ("repeat_x -1 = " ^ (repeat_x (-1)))
