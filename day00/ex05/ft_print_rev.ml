let ft_print_rev s =
    let rec loop s n =
        begin
            if n >= 0 then
                begin
                    begin
                        let current_char = String.get s n in
                            print_char current_char
                    end
                    ; loop s (n - 1)
                end
        end
    in
    let len = String.length s in
    loop s (len - 1)
    ; print_char '\n'
