let is_lower c =
    let c_number = int_of_char c in
    let a_number = int_of_char 'a' in
    let z_number = int_of_char 'z' in
    (c_number >= a_number && c_number <= z_number)

let is_upper c =
    let c_number = int_of_char c in
    let am_number = int_of_char 'A' in
    let zm_number = int_of_char 'Z' in
    (c_number >= am_number && c_number <= zm_number)

let ft_rot_n n s =
    let rot c = begin
        if is_lower c then
            char_of_int ((((int_of_char c) - (int_of_char 'a') + n) mod 26) + (int_of_char 'a'))
        else if is_upper c then
            char_of_int ((((int_of_char c) - (int_of_char 'A') + n) mod 26) + (int_of_char 'A'))
        else
            c
    end in String.map rot s
