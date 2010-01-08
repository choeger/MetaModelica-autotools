(* cps/cps_usages.sig *)

signature CPS_USAGES =
  sig

    structure CPS	: CPS
    val update		: CPS.module -> CPS.module

  end (* signature CPS_USAGES *)
