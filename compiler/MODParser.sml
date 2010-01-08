(* MODParser.sml *)

structure MODParser =
  JoinWithArg(
		  structure Lex = MODLex
	      structure ParserData = MODLrVals.ParserData
	      structure LrParser = LrParser
	      structure MOToRML = MOToRML);
