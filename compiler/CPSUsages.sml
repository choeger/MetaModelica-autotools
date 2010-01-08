(* CPSUsages.sml *)

structure CPSUsages = 
	CPSUsagesFn(
		structure Util = Util 
		structure CPS = CPS 
		structure CPSUseful = CPSUseful);
