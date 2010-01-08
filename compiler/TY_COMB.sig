(* static/tycomb.sig *)

signature TY_COMB =
  sig

    structure Ty	: TY
    type tycomb
    val absBvars	: Ty.tyvar list * Ty.ty -> tycomb
    val absAll		: Ty.ty -> Ty.tyvar list * tycomb
    val absNone		: Ty.ty -> tycomb
    val apply		: tycomb * Ty.ty list -> Ty.ty

  end (* signature TYPE_COMB *)
