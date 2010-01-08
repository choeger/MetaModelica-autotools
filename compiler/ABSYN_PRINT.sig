(* absyn/absyn_print.sig *)

signature ABSYN_PRINT =
  sig

    structure Absyn	: ABSYN
    val printGoal      : TextIO.outstream * Absyn.goal -> unit
    val printModule	   : TextIO.outstream * Absyn.module -> unit
    val printInterface : TextIO.outstream * Absyn.module -> unit
    val printDependencies : TextIO.outstream * Absyn.module -> unit
        
  end (* signature ABSYN_PRINT *)
