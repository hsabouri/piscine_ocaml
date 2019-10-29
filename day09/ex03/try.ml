module Try =
struct
	type 'a t =
		| Success of 'a
		| Failure of exn

	let return v = Success v

	let bind t f = match t with
		| Success t -> f t
		| Failure e -> Failure e

	let recover t f = match t with
		| Failure e -> f e
		| Success t -> Success t

	exception BadFilter

	let filter t f = match t with
		| Success t -> (match f t with
			| true -> Success t
			| false -> Failure BadFilter
		)
		| Failure e -> Failure e

	let flatten t_t = match t_t with
		| Success t -> t
		| Failure e -> Failure e
end

let () =
	let res = try Try.Success (2342 * 10) with any -> Try.Failure any in
	let res = Try.bind res (fun x ->
		print_endline ("x: 2340 * 10 = Success(" ^ string_of_int x ^ ")")
		; Try.Success x
	) in
	let res = Try.bind res (fun x ->
		try Try.Success (x / 0) with any -> Try.Failure any
	) in
	let res = Try.recover res (fun x ->
		print_endline ("x: x / 0 = Failure(Divide_by_zero)")
		; Try.Success 0
	) in
	let res = Try.filter res (fun x -> x = 0) in
	let res = Try.bind res (fun x ->
		print_endline ("filter x: 0 with (fun -> x = 0) = Success(" ^ string_of_int x ^ ")")
		; Try.Success x
	) in
	let res = Try.filter res (fun x -> x <> 0) in
	ignore (
		Try.recover res (fun _ ->
			print_endline ("filter x: 0 with (fun -> x <> 0) = Failure(BadFilter)")
			; Try.Success 0
		)
	)
