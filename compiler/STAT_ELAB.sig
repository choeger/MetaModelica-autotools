(* static/statelab.sig *)

signature STAT_ELAB =
  sig

	structure Absyn  : ABSYN
	
    type repository 
    
    val checkModule	 : 
			TextIO.outstream option * 
			(Absyn.module * repository)
			-> unit
    val checkProgram : 
			TextIO.outstream option * 
			(Absyn.module list * repository)
			-> unit

  end (* signature STAT_ELAB *)
