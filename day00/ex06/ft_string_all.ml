let ft_string_all predicate s =
    let len = String.length s in
    let rec loop predicate s n = begin
        if n == len then
            true
        else
            begin
                let current_char = String.get s n in
                if predicate current_char then
                    loop predicate s (n + 1)
                else
                    false
            end
    end in
    loop predicate s 0
