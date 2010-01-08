(* CPS.sml *)

structure CPS =
  CPSFn(
	structure Util = Util
	structure Source = Source
	structure ConRep = ConRep);
