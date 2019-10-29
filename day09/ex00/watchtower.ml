type hour = int

let zero = 12

let reborn hour = match hour with
	| 0 -> zero
	| any -> any

let neg_pos hour = match hour with
	| hour when hour <= 0 -> zero + (hour mod zero)
	| hour -> hour

let add a b =
	reborn ((neg_pos (a + b)) mod zero)

let sub a b =
	reborn ((neg_pos (a - b)) mod zero)
