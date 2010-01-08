(* MaskToC.sml *)

structure MaskToC =
  MaskToCFn(structure Code = Switch
	    structure SwitchIntras = SwitchIntras
	    structure CodeToC = SwitchCodeToC);

