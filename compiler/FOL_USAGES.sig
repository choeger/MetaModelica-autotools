(* fol/fol_usages.sig *)

signature FOL_USAGES =
  sig

    structure FOL	: FOL
    val update		: FOL.module -> unit

  end (* signature FOL_USAGES *)
