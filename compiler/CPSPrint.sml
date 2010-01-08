(* CPSPrint.sml *)

structure CPSPrint =
  CPSPrintFn(structure MakeString = MakeString
	     structure Util = Util
	     structure PP = PP
	     structure CPS = CPS);
