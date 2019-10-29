let ft_print_comb () =
    let rec loop h d u = begin
        print_int h
        ; print_int d
        ; print_int u
        ; begin
            if h <> 7 || d <> 8 || u <> 9 then
                print_string ", "
        end
        ; if u < 9 then loop h d (u + 1)
        else if d < 8 then loop h (d + 1) (d + 2)
        else if h < 7 then loop (h + 1) (h + 2) (h + 3)
    end in
    loop 0 1 2
    ; print_char '\n'
