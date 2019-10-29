type 'a tree =
	| Nil
	| Node of 'a * 'a tree * 'a tree

let rec size (t:'a tree) = match t with
	| Node (_, left, right) -> 1 + (size left) + (size right)
	| Nil -> 0

let rec height (t: 'a tree) = match t with
	| Node (_, left, right) -> 1 + (max (height left) (height right))
	| Nil -> 0

let draw_tree (t:string tree) = ()
	(* TODO *)

let () =
	let t1 = Nil in
	let t2 = Node ("F", Nil, Nil) in
	let t3 = Node ("G", Nil, Nil) in
	let t4 = Node ("G", Nil, Node ("B", Nil, Nil)) in
	let t5 = Node ("H", Node ("N", Nil, Nil), Nil) in
	let t0 = Node ("6", Node ("T", t2, t3), Node ("Y", t4, t5)) in
	let t0_1 = Node ("6", Node ("T", Nil, Nil), Node ("Y", t4, t5)) in
	print_endline ("size t1: (Nil) = " ^ string_of_int (size t1))
	; print_endline ("size t2: Node ('F', Nil, Nil) = " ^ string_of_int (size t2))
	; print_endline ("size t3: Node ('G', Nil, Nil) = " ^ string_of_int (size t3))
	; print_endline ("size t4: Node ('G', Nil, Node ('B', Nil, Nil)) = " ^ string_of_int (size t4))
	; print_endline ("size t5: Node ('H', Node ('N', Nil, Nil), Nil) = " ^ string_of_int (size t5))
	; print_endline ("size t0: Node ('6', Node ('T', t2, t3), Node ('Y', t4, t5)) = " ^ string_of_int (size t0))
	; print_endline ("height t2: Node ('F', Nil, Nil) = " ^ string_of_int (height t2))
	; print_endline ("height t5: Node ('H', Node ('N', Nil, Nil), Nil) = " ^ string_of_int (height t5))
	; print_endline ("height t0_1: Node ('6', Node ('T', Nil, Nil), Node ('Y', t4, t5)) = " ^ string_of_int (height t0_1))
