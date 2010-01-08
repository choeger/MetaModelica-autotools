(* cps/cps_optim.sig *)

signature CPS_OPTIM =
  sig

    structure CPS	: CPS
    val optimize	: TextIO.outstream option * CPS.module -> CPS.module

  end (* signature CPS_OPTIM *)
