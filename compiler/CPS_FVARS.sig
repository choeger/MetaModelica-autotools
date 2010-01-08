(* cps/cps_fvars.sig *)

signature CPS_FVARS =
  sig

    structure CPS	: CPS
    val update		: CPS.module -> unit

  end (* signature CPS_FVARS *)
