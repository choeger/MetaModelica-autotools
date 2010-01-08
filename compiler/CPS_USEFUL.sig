(* cps/cps_useful.sig *)

signature CPS_USEFUL =
  sig

    structure CPS	: CPS
    val useful		: CPS.module -> CPS.module

  end (* signature CPS_USEFUL *)
