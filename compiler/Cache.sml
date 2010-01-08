(* Cache.sml *)

structure Cache = CacheFn(
	structure StrDict = StrDict
	structure Util    =  Util
	);
