(* PERSISTENTParser.sml *)

structure PERSISTENTParser =
  JoinWithArg(structure Lex = PERSISTENTLex
	      structure ParserData = PERSISTENTLrVals.ParserData
	      structure LrParser = LrParser);
