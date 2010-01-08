(* TransEnv.sml *)

structure TransEnv =
  TransEnvFn(structure Util = Util
	     structure StrDict = StrDict
	     structure CPS = CPS);
