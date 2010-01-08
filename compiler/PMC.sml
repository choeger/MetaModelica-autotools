(* PMC.sml *)


structure PMC =
  PMCFn(
    structure Util = Util
	structure CPS = CPS
	structure Control =  Control);
