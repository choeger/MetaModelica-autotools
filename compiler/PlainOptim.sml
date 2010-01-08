(* PlainOptim.sml *)


structure PlainOptim =
  CodeOptimFn(structure Util = Util
	      structure Code = Plain
	      structure CodeFVars = PlainFVars
	      structure Control = Control);
