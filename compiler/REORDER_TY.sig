(* reorder/reorder-ty.sig *)

signature REORDER_TY =
  sig

    structure Absyn	: ABSYN

    datatype tybnd	= TYPBND of Absyn.typbind
			| DATBND of Absyn.datbind

    datatype tydec	= TYPDEC of Absyn.typbind
			| DATDEC of Absyn.datbind list * Absyn.typbind list

    val reorderTyBnds	: Absyn.Source.source * tybnd list -> tydec list

  end (* signature REORDER_TY *)
