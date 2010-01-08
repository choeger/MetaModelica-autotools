(* PERSISTENTParse.sml *)

structure PERSISTENTParse =
  PERSISTENTParseFn(
	  structure Absyn = Absyn
	  structure PERSISTENTParser = PERSISTENTParser
	  structure Tokens = PERSISTENTLrVals.Tokens
	  structure LexArgSimple = LexArgSimple
	  structure Cache = Cache);

