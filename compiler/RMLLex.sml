(* RMLLex.sml *)
structure RMLLex =
  RMLLexFn(structure Tokens = RMLLrVals.Tokens
	   structure LexArg = LexArg
	   structure MakeString = MakeString
	   structure LexUtil = LexUtil
	   structure Cache = Cache);