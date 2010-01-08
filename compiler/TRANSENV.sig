(* fol2cps/transenv.sig *)

signature TRANSENV =
  sig

    structure StrDict	: STR_DICT
    structure CPS	: CPS
    structure ConRep	: CONREP    
    sharing ConRep	= CPS.ConRep    

    datatype translation= DEF of {def: CPS.def, tieknot: unit->unit}
			| LIT of CPS.literal
			| CON of {rep: ConRep.conrep, nrcons: int}
			| VAR of CPS.trivexp

    val tenv0		: translation StrDict.dict
    val menv0		: translation StrDict.dict StrDict.dict

  end (* signature TRANSENV *)
