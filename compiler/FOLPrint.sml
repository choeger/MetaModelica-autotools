(* FOLPrint.sml *)

structure FOLPrint =
  FOLPrintFn(structure MakeString = MakeString
	     structure Util = Util
	     structure PP = PP
	     structure FOLUsages = FOLUsages);
