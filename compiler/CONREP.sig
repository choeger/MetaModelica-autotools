(* conrep/conrep.sig *)

signature CONREP =
  sig

    (* filename, pos start, pos end *)
	datatype info = INFO of int * int

    datatype ident	= IDENT of string * info

    datatype longid	= LONGID of {module: ident option, name: ident}

    datatype conrep	= INT of int
			| TRANSPARENT
			| BOX of {arity: int, tag: int, name:longid}
			
	val dummyLongIdent : longid
	val dummyInfo : info	
    val identName	    : ident  -> string
    val longIdentName   : longid -> string    
    val fixName         : longid -> string

  end (* signature CONREP *)
