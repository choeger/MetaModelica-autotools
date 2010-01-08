(* CPSToPlain.sml *)

structure CPSToPlain =
  CPSToCodeFn(structure MakeString = MakeString
	      structure Util = Util
	      structure CPS = CPS
	      structure CPSUsages = CPSUsages
	      structure CPSFVars = CPSFVars
	      structure Code = Plain);
