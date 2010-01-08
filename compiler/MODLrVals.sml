(* MODLrVals.sml *)

structure MODLrVals =
  MODLrValsFn(structure Token = LrParser.Token
	      structure Absyn = Absyn
	      structure LexArg = LexArg
	      structure Cache = Cache);
