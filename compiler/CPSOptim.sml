(* CPSOptim.sml *)

structure CPSOptim =
  CPSOptimFn(structure Util = Util
	     structure CPS = CPS
	     structure CPSPrint = CPSPrint
	     structure CPSUsages = CPSUsages);
