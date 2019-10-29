let repeat_string ?(rep="x") n =
    if n < 0 then
        "Error"
    else
        let rec loop n s = begin
            if n > 0 then
                loop (n - 1) (s ^ rep)
            else
                s
        end in
        loop n ""

let () =
    let pulp = "And you will know My name is the Lord when I lay my vengeance upon thee" in
    let pulp2 = "And I will strike down upon thee with great vengeance and furious Anger" in
    print_endline ("repeat_string 10 = " ^ (repeat_string 10))
    ; print_endline ("repeat_string 4 = " ^ (repeat_string 4))
    ; print_endline ("repeat_string 1 = " ^ (repeat_string 1))
    ; print_endline ("repeat_string 0 = " ^ (repeat_string 0))
    ; print_endline ("repeat_string -1 = " ^ (repeat_string (-1)))
    ; print_endline ("repeat_string \"Hey! \" 10 = " ^ (repeat_string ~rep:"Hey! " 10))
    ; print_endline ("repeat_string \"" ^ pulp2 ^ "\" 4 = " ^ (repeat_string ~rep:pulp2 4))
    ; print_endline ("repeat_string \"" ^ pulp ^ "\" 1 = " ^ (repeat_string ~rep:pulp 1))
    ; print_endline ("repeat_string \"Any string\" 0 = " ^ (repeat_string ~rep:"Any string" 0))
    ; print_endline ("repeat_string \"Big Mac\" -1 = " ^ (repeat_string ~rep:"Big Mac" (-1)))
    ; print_endline ("repeat_string \"\" 10 = " ^ (repeat_string ~rep:"" 10))
    ; print_endline ("repeat_string \"\" 1 = " ^ (repeat_string ~rep:"" 1))
    ; print_endline ("repeat_string \"\" -1 = " ^ (repeat_string ~rep:"" (-1)))
