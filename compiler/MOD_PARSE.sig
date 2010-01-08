(* MOD_PARSE.sig *)

signature MOD_PARSE =
  sig

    structure Absyn	: ABSYN
    
    type repository

    val parseModule	    : string * repository -> Absyn.program 
    val parseInterface	: string * repository -> Absyn.program
    	
  end (* signature MOD_PARSE *)
