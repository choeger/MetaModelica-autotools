(* Switch.sml *)


structure Switch =
	SwitchFn(
		structure MakeString = MakeString
		structure Source = Source
		structure ConRep = ConRep
		structure Mangle = Mangle);
