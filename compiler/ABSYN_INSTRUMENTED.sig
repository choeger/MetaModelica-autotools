(* absyn/absyn_instrumented.sig *)
(* the signature of the structure that does the instrumentation *)

signature ABSYN_INSTRUMENTED =
  sig

    structure Absyn	: ABSYN
    val instrument	: string * Absyn.module -> Absyn.module
    val getPatAsString : Absyn.pat -> string
    val getInfoFromPat : Absyn.pat -> Absyn.info
    val getInfoFromExp : Absyn.exp -> Absyn.info
    val getExpAsString : Absyn.exp -> string 

    val getExpsFromExp     : Absyn.exp -> Absyn.exp list
	val getExpsFromExpStar : Absyn.exp list -> Absyn.exp list

    val getExpsFromPat     : Absyn.pat -> Absyn.exp list
	val getExpsFromPatStar : Absyn.pat list -> Absyn.exp list
					    	
    val getAllResultExps   : Absyn.result -> Absyn.exp list				
    val getResultExps      : Absyn.result -> Absyn.exp list
				    
  end (* signature ABSYN_INSTRUMENTED *)
