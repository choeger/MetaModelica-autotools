(* DiffToC.sml *)

structure DiffToC =
  DiffToCFn(structure Code = Switch
	    structure SwitchIntras = SwitchIntras
	    structure CodeToC = SwitchCodeToC);
