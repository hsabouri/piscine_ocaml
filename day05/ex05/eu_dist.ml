let eu_dist af bf =
	let sum = ref 0.0 in
	for i = 0 to (Array.length af) - 1 do
		sum := !sum +. ((af.(i) +. bf.(i)) ** 2.0)
	done
	; sqrt(!sum)

let () =
	let (af, bf) = ([|0.0 ; 0.0|] , [|1.0 ; 1.0|]) in
	let (af2, bf2) = ([|0.0 ; 0.0|] , [|0.0 ; 10.0|]) in
	let dis = eu_dist af bf in
	let dis2 = eu_dist af2 bf2 in
	print_endline (string_of_float dis)
	; print_endline (string_of_float dis2)
