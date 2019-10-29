let my_sleep () = Unix.sleep 1

let sleep_n_times n = match n with
	| n when n > 0 -> begin
			for _ = 1 to n do
				my_sleep ()
			done
		end
	| _ -> ()

let () =
	if Array.length Sys.argv >= 2 then begin
		try sleep_n_times (int_of_string Sys.argv.(1)) with
			| _ -> ()
	end else
		()

(*
ocamlbuild -lib unix micronap.native
*)
