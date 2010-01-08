(* cps/cps_print.sig *)

signature CPS_PRINT =
  sig

    structure CPS	: CPS
    val printModule	: TextIO.outstream * CPS.module -> unit

  end (* signature CPS_PRINT *)
