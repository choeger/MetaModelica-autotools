(* Interp.sml *)

structure Interp =
  InterpFn(structure Util = Util
	   structure InterpCore = InterpCore);
