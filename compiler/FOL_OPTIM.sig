(* fol/fol_optim.sig *)

signature FOL_OPTIM =
  sig

    structure FOL	: FOL
    val optimize	: TextIO.outstream option * FOL.module -> FOL.module

  end (* signature FOL_OPTIM *)
