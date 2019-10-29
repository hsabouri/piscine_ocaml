type project = string * string * int

let zero = ("", "", 0)

let combine (project_a, status_a, grade_a) (project_b, status_b, grade_b) =
	let mean = (grade_a + grade_b) / 2 in
	let status = if mean < 80 then "failed" else "success" in
	(project_a ^ project_b, status, mean)

let fail (project, _, _) = (project, "failed", 0)

let success (project, _, _) = (project, "success", 80)
