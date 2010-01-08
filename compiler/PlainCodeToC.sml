(* PlainCodeToC.sml *)

structure PlainCodeToC =
  CodeToCFn(structure MakeString = MakeString
	    structure Util = Util
	    structure Code = Plain
	    structure Control = Control
	      );
