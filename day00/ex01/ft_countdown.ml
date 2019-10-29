let ft_countdown n =
    let print_int_endline n =
        begin
            print_int n ;
            print_char '\n'
        end
    in
    let rec loop n =
        if n <= 0 then
            print_int_endline 0
        else
            begin
                print_int_endline n ;
                loop (n - 1)
            end
    in
    loop n
