(* MODParse.sml *)

structure MODParse =
  MODParseFn(
	  structure Absyn = Absyn
	  structure MODParser = MODParser
	  structure Tokens = MODLrVals.Tokens
	  structure LexArg = LexArg
	  structure Util = Util
	  structure Cache = Cache);

