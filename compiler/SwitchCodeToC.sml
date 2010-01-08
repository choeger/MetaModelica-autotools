(* SwitchCodeToC.sml *)

structure SwitchCodeToC =
  CodeToCFn(structure MakeString = MakeString
	    structure Util = Util
	    structure Code = Switch
	    structure Control = Control
	      );
