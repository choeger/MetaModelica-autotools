(* fol/fol_print.sig *)

signature FOL_PRINT =
  sig

    structure FOL	: FOL
    val printModule	: TextIO.outstream * FOL.module -> unit

  end (* signature FOL_PRINT *)
