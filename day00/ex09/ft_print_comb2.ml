let print_int_2 n =
    begin if n < 10 then
        print_char '0'
    end
    ; print_int n

let ft_print_comb2 () =
    let rec loop left right = begin
        print_int_2 left
        ; print_char ' '
        ; print_int_2 right
        ; begin if left <> 98 || right <> 99 then
            print_char ',' ; print_char ' '
        end
        ; if right < 99 then loop left (right + 1)
        else if left < 98 then loop (left + 1) (left + 2)
    end in
    loop 0 1
    ; print_char '\n'

(* let ft_print_comb2 () =
    let rec loop left right = begin
        print_int_2 left
        ; print_char ' '
        ; print_int_2 right
        ; begin if (right + 1) mod 10 == 0 then begin
            print_char '\n'
        end else
            print_char ','
            ; print_char ' '
        end
        ; if right < 99 then loop left (right + 1)
        else if left < 98 then loop (left + 1) (left + 2)
    end in
    loop 0 1
    ; print_char '\n' *)

