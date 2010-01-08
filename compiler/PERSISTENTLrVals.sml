(* PERSISTENTLrVals.sml *)

structure PERSISTENTLrVals =
  PERSISTENTLrValsFn(structure Token = LrParser.Token
	      structure Absyn = Absyn
	      structure LexArgSimple = LexArgSimple
	      structure Cache = Cache);
