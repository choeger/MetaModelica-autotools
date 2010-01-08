(* absyn/absyn_persist.sig *)

signature ABSYN_PERSIST =
  sig

    structure Absyn	: ABSYN
    
    type repository
    
    val serializeModule	: TextIO.outstream * Absyn.module -> unit
    
    val parseModule	    : string * repository -> Absyn.program 
    val parseInterface	: string * repository -> Absyn.program
    
  end (* signature ABSYN_PERSIST *)
