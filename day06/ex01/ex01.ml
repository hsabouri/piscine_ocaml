module StringHash =
		struct
			type t = string
			let equal = String.equal
			let hash s =
				(* SDBM hash function *)
				let rec loop acc s i = match i with
					| i when i < String.length s -> loop (
							(int_of_char (String.get s i)) + (acc lsl 6) + (acc lsl 16) - acc (* lsl -> bitwise left-shift *)
						) s (i + 1)
					| _ -> acc
				in loop 0 s 0

		end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht
