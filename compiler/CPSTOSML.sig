(* cpstosml/cpstosml.sig *)

signature CPSTOSML =
  sig

    structure CPS	: CPS
    val emitModule	: TextIO.outstream * ((string * string option) * CPS.module) -> unit
    val emitInterface	: TextIO.outstream * CPS.module -> unit

  end (* signature CPSTOSML *)
