(* SwitchOptim.sml *)

structure SwitchOptim =
  CodeOptimFn(structure Util = Util
	      structure Code = Switch
	      structure CodeFVars = SwitchFVars
	      structure Control = Control);
