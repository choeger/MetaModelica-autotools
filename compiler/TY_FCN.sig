(* static/tyfcn.sig *)

signature TY_FCN =
  sig

    structure Ty	: TY
    type tyfcn
    val lambda		: Ty.tyvar list * Ty.ty -> tyfcn
    val arity		: tyfcn -> int
    val apply		: tyfcn * Ty.ty list -> Ty.ty
    val admitsEq	: tyfcn -> bool

  end (* signature TY_FCN *)
