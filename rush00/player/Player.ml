type brain = Human | Robot
type t = Color.t * string * brain

let brainToString brain = match brain with
  | Human -> "Human"
  | Robot -> "Robot"

let toString (color, s, _) = Color.applyColorToString color s
let toStringVerbose (color, s, brain) = "Player " ^ toString (color, s, brain) ^ " (" ^ brainToString brain ^")"

let newPlayer color name _3brain =
  match (Color.parseString color) with
    | Ok c -> (c, name, Human)
    | Err _ -> (Color.Green, name, Human)

let playerColor (color, name, brain) = color
let playerName (color, name, brain) = name
