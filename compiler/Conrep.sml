(* conrep/conrep.sml *)

structure ConRep : CONREP =
  struct

    (* filename, pos start, pos end *)
	datatype info = INFO of int * int

    datatype ident	= IDENT of string * info

    datatype longid	= LONGID of {module: ident option, name: ident}

    datatype conrep	= INT of int
			| TRANSPARENT
			| BOX of {arity: int, tag: int, name:longid}
			
    val dummyInfo  = INFO(~1, ~1)
    val dummyIdent = IDENT("$", dummyInfo)
    val dummyLongIdent = LONGID{module=NONE, name=dummyIdent}	
    
    fun identName(IDENT(name,_)) = name
    fun longIdentName(LONGID{module=SOME(IDENT(name1,_)),name=IDENT(name2,_)}) = name1^"."^name2
    |	longIdentName(LONGID{module=NONE,name=IDENT(name2,_)}) = name2    

    fun fixName(LONGID{module=SOME(IDENT(name1,_)),name=IDENT(name2,_)}) = "/* "^name1^"_"^name2^" */"
    |	fixName(LONGID{module=NONE,name=IDENT(name2,_)}) = if (name2 = "$" ) then "" else "/* "^name2^" */"
    

  end (* structure ConRep *)
