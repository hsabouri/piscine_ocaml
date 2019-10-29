let leibniz_pi delta =
    if delta < 0.0 then
        -1
    else
        let rec iter_of_delta acc i check compute = begin
            if check acc then
                i
            else
                iter_of_delta (acc +. (compute i)) (i + 1) check compute
        end
        in
        let compute_i i = begin
            let top = if i mod 2 == 0 then (1.0) else (-1.0) in
            let i_float = float_of_int i in
            top /. (2.0 *. i_float +. 1.0)
        end in
        let check delta v =
            let absolute v = begin
                if v >= 0.0 then (v) else (-.v)
            end in
            absolute ((4.0 *. v) -. (4.0 *. (atan 1.0))) <= delta
        in
        iter_of_delta 0.0 0 (check delta) compute_i

let () =
    print_endline ("leibniz_pi 1.0 = " ^ string_of_int (leibniz_pi 1.0))
    ; print_endline ("leibniz_pi 0.0001 = " ^ string_of_int (leibniz_pi 0.0001))
    ; print_endline ("leibniz_pi 0.00000029854 = " ^ string_of_int (leibniz_pi 0.00000029854))
    ; print_endline ("leibniz_pi -10 = " ^ string_of_int (leibniz_pi (-10.0)))

