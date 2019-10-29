let ft_test_sign a =
	print_string "ft_test_sign " ;
	print_int a ;
	print_string " = " ;
	if a >= 0 then
		print_endline "positive"
	else
        print_endline "negative"
