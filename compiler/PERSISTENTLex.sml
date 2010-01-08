(* PERSISTENTLex.sml *)
structure PERSISTENTLex =
  PERSISTENTLexFn(structure Tokens = PERSISTENTLrVals.Tokens
	   structure LexArgSimple = LexArgSimple
	   structure MakeString = MakeString
	   structure LexUtil = LexUtil
	   structure Cache = Cache);