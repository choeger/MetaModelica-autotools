(* RMLParser.sml *)

structure RMLParser = JoinWithArg(
	structure Lex = RMLLex
	structure ParserData = RMLLrVals.ParserData
	structure LrParser = LrParser
	structure Control = Control);
