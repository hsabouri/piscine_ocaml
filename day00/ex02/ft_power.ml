let rec ft_power a b =
    if b == 1 then
        a
    else if b <= 0 then
        1
    else if a == 0 then
        0
    else
        a * ft_power a (b - 1)
