let () =
	let s = "Hello world ! \n Line 2" in
	let s_rot = Cipher.rot42 s in
	let s_unrot = Uncipher.unrot42 s_rot in
	let s_2 = "Hello world ! \n Line 2" in
	let s_2_rot = Cipher.caesar s 1 in
	let s_2_unrot = Uncipher.uncaesar s_2_rot 1 in
	let s_3 = "Hello world ! \n Line 2" in
	let s_3_xor = Cipher.xor s 1 in
	let s_3_unxor = Cipher.xor s_3_xor 1 in
	let s_4 = "Hello world ! \n Line 2" in
	let s_4_xor = Cipher.xor s 823529 in
	let s_4_unxor = Cipher.xor s_4_xor 823529 in
	let s_crypt = "This is the baseline for testing crypt !!" in
	let s_crypted = Cipher.ft_crypt s_crypt [Cipher.rot42 ; (fun s -> Cipher.caesar s 1238745) ; (fun s -> Cipher.xor s 1014)] in
	let s_decrypted = Uncipher.ft_uncrypt s_crypted [Uncipher.unrot42 ; (fun s -> Uncipher.uncaesar s 1238745) ; (fun s -> Cipher.xor s 1014)] in
	let s_2_crypt = "This is the baseline for testing crypt !!" in
	let s_2_crypted = Cipher.ft_crypt s_2_crypt [] in
	let s_2_decrypted = Uncipher.ft_uncrypt s_2_crypted [] in
	let s_3_crypt = "This is the baseline for testing crypt !!" in
	let s_3_crypted = Cipher.ft_crypt s_3_crypt [Cipher.rot42 ; (fun s -> Cipher.xor s 101) ; (fun s -> Cipher.caesar s 1238745) ; (fun s -> Cipher.xor s 1014) ; (fun s -> Cipher.caesar s 3856)] in
	let s_3_decrypted = Uncipher.ft_uncrypt s_3_crypted [Uncipher.unrot42 ; (fun s -> Cipher.xor s 101) ; (fun s -> Uncipher.uncaesar s 1238745) ; (fun s -> Cipher.xor s 1014) ; (fun s -> Uncipher.uncaesar s 3856)] in
	print_endline   ( "baseline   : " ^ s)
	; print_endline ( "rot42      : " ^ s_rot)
	; print_endline ( "unrot42    : " ^ s_unrot)
	; print_endline "--------------------------"
	; print_endline ( "baseline   : " ^ s_2)
	; print_endline ( "caesar 1   : " ^ s_2_rot)
	; print_endline ( "uncaesar 1 : " ^ s_2_unrot)
	; print_endline "--------------------------"
	; print_endline ( "baseline   : " ^ s_3)
	; print_endline ( "xor 1      : " ^ s_3_xor)
	; print_endline ( "xor 1      : " ^ s_3_unxor)
	; print_endline "--------------------------"
	; print_endline ( "baseline   : " ^ s_4)
	; print_endline ( "xor 823529 : " ^ s_4_xor)
	; print_endline ( "xor 823529 : " ^ s_4_unxor)
	; print_endline "--------------------------"
	; print_endline ( "baseline   : " ^ s_crypt)
	; print_endline ( "ft_crypt [...]   : " ^ s_crypted)
	; print_endline ( "ft_uncrypt [...] : " ^ s_decrypted)
	; print_endline "--------------------------"
	; print_endline ( "baseline   : " ^ s_2_crypt)
	; print_endline ( "ft_crypt []   : " ^ s_2_crypted)
	; print_endline ( "ft_uncrypt [] : " ^ s_2_decrypted)
	; print_endline "--------------------------"
	; print_endline ( "baseline           : " ^ s_3_crypt)
	; print_endline ( "ft_crypt [.....]   : " ^ s_3_crypted)
	; print_endline ( "ft_uncrypt [.....] : " ^ s_3_decrypted)
