(* Absyn.sml *)

structure Absyn =
  AbsynFn(structure MakeString = MakeString
	  structure Source = Source
	    );
