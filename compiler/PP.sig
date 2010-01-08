(* util/pp.sig *)

signature PP =
  sig

    datatype breaks	= CONSISTENT | INCONSISTENT
    datatype token	= STRING of string
			| BREAK of
			    {blankSpace	: int,	(* number of spaces per blank *)
			     offset	: int}	(* indent for overflow lines *)
			| BEGIN of
			    {offset	: int,	(* indent for this group *)
			     breakType	: breaks}
			| END

    val lineBreak	: token

    type state
    val init		: TextIO.outstream * int -> state
    val emit		: state * token -> unit
    val close		: state -> unit

  end (* signature PP *)
