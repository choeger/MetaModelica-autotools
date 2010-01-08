(* SwitchFVars.sml *)


structure SwitchFVars =
  CodeFVarsFn(structure Util = Util
	      structure Code = Switch);
