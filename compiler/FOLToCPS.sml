(* FOLToCPS.sml *)

structure FOLToCPS =
  FOLToCPSFn(structure Util = Util
	     structure FOLUsages = FOLUsages
	     structure PMC = PMC
	     structure TransEnv = TransEnv
	     structure Control = Control
	       );
