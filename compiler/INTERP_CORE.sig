(* interp/interpcore.sig *)

signature INTERP_CORE =
  sig

    structure Absyn	: ABSYN
    (* 
    structure IdentDict : ORD_DICT (* = Absyn.IdentDict *)
    sharing type IdentDict.dict = Absyn.IdentDict.dict
    *)

    type loc
    type prim
    datatype value	= LIT of Absyn.lit
			| TUPLE of value list
			| STRUCT of string * value list
			| LOC of loc
			| CLOSURE of {clause : Absyn.clause,
				      ME : value Absyn.IdentDict.dict Absyn.IdentDict.dict,
				      VE : value Absyn.IdentDict.dict,
				      VE_rec : value Absyn.IdentDict.dict}
			| PRIM of prim
    type state
    type marker

    val equal		: value * value -> bool
    val marker		: state -> marker
    val restore		: marker * state -> state

    val list2value	: ('a -> value) -> 'a list -> value

    datatype result	= SUCCESS of value list * state
			| FAILURE of state
    val APPLY		: prim * value list * state -> result

    val s_init		: state
    val VE_init		: value Absyn.IdentDict.dict
    val ME_init		: value Absyn.IdentDict.dict Absyn.IdentDict.dict

  end (* signature INTERP_CORE *)
