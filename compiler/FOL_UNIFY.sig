(* fol/fol_unify.sig *)

signature FOL_UNIFY =
  sig

    structure FOL		: FOL
    val equalVarLists	: FOL.var list * FOL.var list -> bool
    val unifyPatLists	: FOL.pat list * FOL.pat list -> bool
    val unifyConjs		: FOL.conj * FOL.conj -> bool
    val exposeNames		: FOL.longid -> string option * string
    val exposeLongId	: FOL.longid option -> (string option * string) option

  end (* signature FOL_UNIFY *)
