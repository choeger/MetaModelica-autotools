(* AbsynPersist *)

structure AbsynPersist =
  AbsynPersistFn(
	       structure MakeString = MakeString
	       structure Util = Util
	       structure Absyn = Absyn
	       structure Control = Control
	       structure PERSISTENTParse = PERSISTENTParse
	       structure Source = Source
	       structure Control = Control
	       structure Cache = Cache);

