(* RMLParse.sml *)

structure RMLParse =
  RMLParseFn(
	  structure Absyn = Absyn
	  structure RMLParser = RMLParser
	  structure Tokens = RMLLrVals.Tokens
	  structure LexArg = LexArg
	  structure Control = Control
	  structure Cache = Cache
	  structure Reorder = Reorder
	  );

