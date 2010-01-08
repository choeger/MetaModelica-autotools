(* PlainToC.sml *)

structure PlainToC =
  PlainToCFn(structure Util = Util
	     structure Code = Plain
	     structure CodeToC = PlainCodeToC
	     structure Control = Control);

