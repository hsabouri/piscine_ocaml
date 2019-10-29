
let () =
	let sideKick = new People.people "Amy" in
	let doctor = new Doctor.doctor "Doctor Who" sideKick 700 in
	let sideKickArmy = new Army.army [ sideKick ] in
	let army = new Army.army [ new Dalek.dalek ] in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let army = army#add new Dalek.dalek in
	let _ = sideKickArmy#delete in
	doctor#talk
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver
	; let army = army#delete in
	doctor#use_sonic_screwdriver

