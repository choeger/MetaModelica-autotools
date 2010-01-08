(* main/link.sml *)

structure CPSFVars = 
	CPSFVarsFn(
		structure MakeString = MakeString 
		structure Util = Util 
		structure CPS = CPS);
