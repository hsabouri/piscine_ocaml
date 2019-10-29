let ft_is_palindrome s =
    let len = String.length s in
    let rec loop s left right = begin
        if right <= left then
            true
        else if (String.get s left) == (String.get s right) then
            loop s (left + 1) (right - 1)
        else
            false
    end in
    loop s 0 (len - 1)
