(* foltocps/foltocps.sig *)

signature FOLTOCPS =
  sig

    structure FOL	: FOL
    structure CPS	: CPS

    val translate	: FOL.module -> CPS.module

  end (* signature FOLTOCPS *)
