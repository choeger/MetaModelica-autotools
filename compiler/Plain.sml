(* Plain.sml *)

structure Plain =
  PlainFn(
  	structure MakeString = MakeString
  	structure Source = Source
	  structure ConRep = ConRep
	  structure Mangle = Mangle);
