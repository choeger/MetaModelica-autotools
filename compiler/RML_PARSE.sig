(* RML_PARSE.sig *)

signature RML_PARSE =
  sig

    structure Absyn	: ABSYN
    
    type repository 

    val parseModule	    : string * repository -> Absyn.program
    val parseInterface	: string * repository -> Absyn.program
    	
  end (* signature RML_PARSE *)
