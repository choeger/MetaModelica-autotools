(* static/tyscheme.sig *)

signature TY_SCHEME =
  sig

    structure Ty	: TY
    type tyscheme
    val genAll		: Ty.ty -> tyscheme
    val genNone		: Ty.ty -> tyscheme
    val instFree	: tyscheme -> Ty.ty
    val instRigid	: tyscheme -> Ty.ty

  end (* signature TY_SCHEME *)
