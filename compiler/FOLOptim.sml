(* FOLOptim.sml *)

structure FOLOptim =
  FOLOptimFn(structure Util = Util
	     structure FOLPrint = FOLPrint
	     structure FOLUnify = FOLUnify);
