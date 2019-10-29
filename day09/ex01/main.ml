let print_proj (project, status, grade) =
	Printf.printf "Project: %s, status: %s, grade: %d\n" project status grade

let () =
	let proj1 = App.success ("Piscine ocaml", "", 0) in
	let proj2 = App.fail ("Gomoku", "", 0) in
	let proj3 = App.success ("Printf", "", 0) in
	print_proj proj1
	; print_proj proj2
	; print_proj (App.combine proj1 proj2)
	; print_proj (App.combine proj1 proj3)
