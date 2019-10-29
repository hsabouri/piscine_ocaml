
let () =
	Random.self_init () ;
	let sideKick = new People.people "Amy" in
	let doctor = new Doctor.doctor "Doctor Who" sideKick 700 in
	let sideKickArmy = new Army.army [ sideKick ] in
	let sideKickArmy = sideKickArmy#add (new People.people "Jeff") in
	let sideKickArmy = sideKickArmy#add (new People.people "Kevin") in
	let sideKickArmy = sideKickArmy#add (new People.people "Elise") in
	let sideKickArmy = sideKickArmy#add (new People.people "Antony") in
	let sideKickArmy = sideKickArmy#add (new People.people "Robert") in
	let sideKickArmy = sideKickArmy#add (new People.people "Jack") in
	let sideKickArmy = sideKickArmy#add (new People.people "Ely") in
	let sideKickArmy = sideKickArmy#add (new People.people "Helena") in
	let sideKickArmy = sideKickArmy#add (new People.people "Rona") in
	let army = new Army.army [ new Dalek.dalek ] in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let galigrey = new Galifrey.galifrey army doctor sideKickArmy in
	galigrey#do_time_war

