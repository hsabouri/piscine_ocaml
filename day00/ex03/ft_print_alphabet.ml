let ft_print_alphabet () =
    let a = int_of_char 'a' in
    let z = int_of_char 'z' in
    let rec loop letter =
        if letter <= z then
            begin
                begin
                    let letter_char = char_of_int letter in
                    print_char letter_char
                end
                ; loop (letter + 1)
            end
    in
    loop a
    ; print_char '\n'
