type 'a tree =
	| Nil
	| Node of 'a * 'a tree * 'a tree

let draw_square x y size =
	let size_half = size / 2 in
	Graphics.moveto (x - size_half) (y - size_half)
	; Graphics.lineto (x + size_half) (y - size_half)
	; Graphics.lineto (x + size_half) (y + size_half)
	; Graphics.lineto (x - size_half) (y + size_half)
	; Graphics.lineto (x - size_half) (y - size_half)

let draw_tree_node (n:'a tree) x y =
	draw_square x y 50
	; draw_square (x - 50) (y + 100) 50
	; draw_square (x + 50) (y + 100) 50
	; Graphics.moveto (x - 50) (y + 25)
	; Graphics.lineto (x - 50) (y + 75)
	; Graphics.moveto x (y + 25)
	; Graphics.lineto (x + 50) (y + 75)
	; Graphics.moveto (x + 10) (y + 20)
	; Graphics.draw_string "value"
	; Graphics.moveto (x - 40) (y + 110)
	; Graphics.draw_string "Nil"
	; Graphics.moveto (x - 40) (y + 110)
	; Graphics.draw_string "Nil"
