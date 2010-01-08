(* MODLex.sml *)
structure MODLex =
  MODLexFn(structure Tokens = MODLrVals.Tokens
	   structure LexArg = LexArg
	   structure MakeString = MakeString
	   structure LexUtil = LexUtil
	   structure Cache = Cache);