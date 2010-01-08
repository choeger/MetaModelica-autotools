(* RMLLrVals.sml *)

structure RMLLrVals =
  RMLLrValsFn(structure Token = LrParser.Token
	      structure Absyn = Absyn
	      structure LexArg = LexArg
	      structure Cache = Cache);
