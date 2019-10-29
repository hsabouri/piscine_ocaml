class galifrey ldalek doctor lpeople = object (self)
	val _dalek_army: Dalek.dalek Army.army = ldalek
	val _doctor: Doctor.doctor = doctor
	val _people_army: People.people Army.army = lpeople

	method do_time_war = match (_dalek_army#is_dead, _people_army#is_dead) with
			| (true, false) -> print_endline "People + Doctor army won the Great Time war despite all bets !"
			| (false, true) -> print_endline "Daleks won, the Doctor is now alone and will be in prison"
			| (false, false) -> ( match Random.int 6 with
				| 0 -> _doctor#use_sonic_screwdriver ; (new galifrey _dalek_army#delete _doctor _people_army)#do_time_war
				| 1 -> _doctor#travel_in_time 10 10; (new galifrey _dalek_army#delete _doctor _people_army)#do_time_war
				| 2 | 3 -> (_dalek_army#get 0)#talk ; (new galifrey _dalek_army _doctor _people_army#delete)#do_time_war
				| _ -> (_people_army#get 0)#talk ; (new galifrey _dalek_army#delete _doctor _people_army#delete)#do_time_war
			)
			| (true, true) -> print_endline "The Doctor is alone, this a not a win for anyone !"

end
