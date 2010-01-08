(* AbsynPrint *)

structure AbsynPrint =
  AbsynPrintFn(structure MakeString = MakeString
	       structure Util = Util
	       structure Absyn = Absyn
	       structure Control = Control);

