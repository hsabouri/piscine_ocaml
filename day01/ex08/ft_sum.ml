let ft_sum f low up =
    if low > up then
        nan
    else
        let rec sum_acc acc f i = begin
            if i > up then
                acc
            else
                sum_acc ((f i) +. acc) f (i + 1)
        end in
        sum_acc 0.0 f low

let () =
	print_endline ("ft_sum (fun x -> float_of_int(x * x)) 1 10 = " ^ (string_of_float (ft_sum (fun x -> float_of_int(x * x)) 1 10)))
	; print_endline ("ft_sum (fun x -> float_of_int(x * x)) 10 1 = " ^ (string_of_float (ft_sum (fun x -> float_of_int(x * x)) 10 1)))
	; print_endline ("ft_sum (fun x -> float_of_int(x)) 0 10 = " ^ (string_of_float (ft_sum (fun x -> float_of_int(x)) 0 10)))
	; print_endline ("ft_sum (fun x -> float_of_int(-x)) 0 10 = " ^ (string_of_float (ft_sum (fun x -> float_of_int(-x)) 0 10)))
