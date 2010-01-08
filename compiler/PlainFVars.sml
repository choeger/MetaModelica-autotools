(* PlainFVars.sml *)

structure PlainFVars =
  CodeFVarsFn(structure Util = Util
	      structure Code = Plain);
