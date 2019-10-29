type 'a tree =
	| Nil
	| Node of 'a * 'a tree * 'a tree

let is_lower (t:'a tree) parent = match t with
	| Node (value, _, _) -> value < parent
	| Nil -> true

let is_upper (t:'a tree) parent = match t with
	| Node (value, _, _) -> value > parent
	| Nil -> true

let rec is_bst (t:'a tree) = match t with
	| Node (value, left, right) when is_lower left value && is_upper right value -> (is_bst left) && (is_bst right)
	| Node (value, left, Nil) when is_lower left value -> is_bst left
	| Node (value, Nil, right) when is_upper right value -> is_bst right
	| Node (_, Nil, Nil) | Nil -> true
	| Node (_, _, _) -> false

let rec height (t: 'a tree) = match t with
	| Node (_, left, right) -> 1 + (max (height left) (height right))
	| Nil -> 0

let is_diff_max d (t:'a tree) =
	let h = height t in
	let rec explore d h acc (t:'a tree) = match t with
		| Node (_, left, right) -> (explore d h (acc + 1) left) && (explore d h (acc + 1) right)
		| Nil -> acc <= h && acc >= h - d
	in
	explore d h 0 t

let is_perfect (t:'a tree) =
	is_bst t && is_diff_max 0 t

let is_balanced (t:'a tree) =
	is_bst t && is_diff_max 1 t

let rec search_bst v (t:'a tree) = match t with
	| Node (value, left, _) when value > v -> search_bst v left
	| Node (value, _, right) when value < v -> search_bst v right
	| Node (value, _, _) when value = v -> true
	| Nil -> false
	| _ -> failwith "You dumb it's not a true BST"

let rec add_bst v (t:'a tree) = match t with
	| Node (value, left, right) when value > v && left = Nil -> Node (value, Node (v, Nil, Nil), right)
	| Node (value, left, right) when value < v && right = Nil -> Node (value, left, Node (v, Nil, Nil))
	| Node (value, left, right) when value > v -> Node (value, add_bst v left, right)
	| Node (value, left, right) when value < v -> Node (value, left, add_bst v right)
	| Nil -> Node (v, Nil, Nil)
	| whatever_the_fuck -> whatever_the_fuck

let rec bst_min (t:'a tree) = match t with
	| Node (value, Nil, _) -> value
	| Node (_, left, _) -> bst_min left
	| Nil -> failwith "Empty tree"

let rec delete_min (t:'a tree) = match t with
	| Node (value, Nil, right) -> right
	| Node (value, left, right) -> Node (value, delete_min left, right)
	| Nil -> Nil

let rec delete_bst v (t:'a tree) = match t with
	| Node (value, Nil, right) when value = v -> right
	| Node (value, left, Nil) when value = v -> left
	| Node (value, Nil, Nil) when value = v -> Nil
	| Node (value, left, right) when value = v -> Node (bst_min right, left, delete_min right)
	| Node (value, left, right) when value < v -> Node (value, left, delete_bst v right)
	| Node (value, left, right) when value > v -> Node (value, delete_bst v left, right)
	| any -> any

let () =
	let bst = Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) in
	let bst_add_100_truth = Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Node (100, Nil, Nil)))) in
	let bst_add_100 = add_bst 100 bst in
	let bst_add_10_truth = Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Node (100, Node (10, Nil, Nil), Nil)))) in
	let bst_add_10 = add_bst 10 bst_add_100 in
	let bst_remove_100 = delete_bst 100 bst_add_10_truth in
	let bst_remove_100_truth = Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Node (10, Nil, Nil)))) in
	let bst_remove_5_truth = Node (6, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Nil, Node(8, Nil, Nil))) in
	let bst_remove_5 = delete_bst 5 bst in
	let nobst = Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) in
	let nobst2 = Node (5, Node (10, Nil, Nil), Node (1234, Nil, Nil)) in
	let noperfect = Node (5, Node (3, Nil, Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) in
	let nobalanced = Node (5, Nil, Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) in
	print_endline "\x1b[32m------------------- IS_BST --------------------\x1b[0m\n"
	; print_endline ("is_bst Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (is_bst bst) )
	; print_endline ("is_bst Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (is_bst nobst) )
	; print_endline ("is_bst Node (5, Node (3, Nil, Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (is_bst nobalanced) )
	; print_endline ("is_bst Nil .   -> " ^ string_of_bool (is_bst Nil) )
	; print_endline "\n\n\x1b[32m----------------- IS_PERFECT ------------------\x1b[0m\n"
	; print_endline ("is_perfect Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (is_perfect bst) )
	; print_endline ("is_perfect Node (5, Node (3, Nil, Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (is_perfect noperfect) )
	; print_endline ("is_perfect Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (is_perfect nobst) )
	; print_endline ("is_perfect Nil .   -> " ^ string_of_bool (is_perfect Nil) )
	; print_endline "\n\n\x1b[32m----------------- IS_BALANCED ------------------\x1b[0m\n"
	; print_endline ("is_balanced Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (is_balanced bst) )
	; print_endline ("is_balanced Node (5, Node (3, Nil, Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (is_balanced noperfect) )
	; print_endline ("is_balanced Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (is_balanced nobst) )
	; print_endline ("is_balanced Node (5, Node (3, Nil, Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (is_balanced nobalanced) )
	; print_endline ("is_balanced Nil .   -> " ^ string_of_bool (is_balanced Nil) )
	; print_endline "\n\n\x1b[32m----------------- SEARCH_BST -------------------\x1b[0m\n"
	; print_endline ("search_bst 4 Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (search_bst 4 bst))
	; print_endline ("search_bst 130 Node (5, Node (3, Node (1, Nil, Nil), Node(4, Nil, Nil)), Node (7, Node (6, Nil, Nil), Node(8, Nil, Nil))) \n .   -> " ^ string_of_bool (search_bst 130 bst))
	; print_endline ("search_bst 1 Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (search_bst 1 nobst) )
	; print_endline ("search_bst 7 Node (5, Node (3, Node (7, Nil, Nil), Node(1, Nil, Nil)), Node (7, Node (8, Nil, Nil), Node(5, Nil, Nil))) \n .   -> " ^ string_of_bool (search_bst 7 nobst) )
	; print_endline ("search_bst 3 Node (5, Node (10, Nil, Nil), Node (1234, Nil, Nil)) \n .   -> " ^ string_of_bool (search_bst 3 nobst2) )
	; print_endline ("search_bst 0 Nil .   -> " ^ string_of_bool (search_bst 0 Nil) )
	; print_endline "\n\n\x1b[32m------------------- ADD_BST --------------------\x1b[0m\n"
	; print_endline ("add 100 -> " ^ (string_of_bool (bst_add_100_truth = bst_add_100)))
	; print_endline ("is_bst << -> " ^ string_of_bool (is_bst bst_add_100))
	; print_endline ("add 10 << -> " ^ (string_of_bool (bst_add_10_truth = bst_add_10)))
	; print_endline ("is_bst << -> " ^ string_of_bool (is_bst bst_add_10))
	; print_endline ("add 1 Nil -> " ^ string_of_bool (Node (1, Nil, Nil) = add_bst 1 Nil))
	; print_endline "\n\n\x1b[32m------------------- ADD_BST --------------------\x1b[0m\n"
	; print_endline ("remove 100 -> " ^ (string_of_bool (bst_remove_100_truth = bst_remove_100)))
	; print_endline ("remove 5 -> " ^ (string_of_bool (bst_remove_5_truth = bst_remove_5)))
	; print_endline ("remove any from Nil -> " ^ (string_of_bool (Nil = delete_bst 5 Nil)))
	; print_endline ("remove 5 from Node (5, Nil, Nil) -> " ^ (string_of_bool (Nil = delete_bst 5 (Node (5, Nil, Nil)))))
	; print_endline ("remove 5 from Node (4, Nil, Nil) -> " ^ (string_of_bool ((Node (4, Nil, Nil)) = delete_bst 5 (Node (4, Nil, Nil)))))
