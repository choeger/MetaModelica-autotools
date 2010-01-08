(* InterpCore.sml *)

structure InterpCore =
  InterpCoreFn(structure MakeString = MakeString
	       structure LexUtil = LexUtil
	       structure Util = Util
	       structure Absyn = Absyn
	       structure IntDict = IntDict);
