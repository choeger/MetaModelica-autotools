(* reorder/reorder-val.sig *)

signature REORDER_VAL =
  sig

    structure Absyn	: ABSYN

    datatype valbnd	= VALBND of Absyn.ident * Absyn.exp
			| RELBND of Absyn.relbind

    datatype valdec	= VALDEC of Absyn.ident * Absyn.exp
			| RELDEC of Absyn.relbind list

    val reorderValBnds	: Absyn.Source.source * valbnd list -> valdec list

  end (* signature REORDER_VAL *)
