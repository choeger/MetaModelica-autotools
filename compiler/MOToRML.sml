(* MOToRML.sml *)

structure MOToRML = MOToRMLFn
	(
		structure Absyn = Absyn
		structure Control = Control
		structure Util = Util
		structure StrDict = StrDict
		structure AbsynPrint = AbsynPrint
		structure Cache = Cache
	);
