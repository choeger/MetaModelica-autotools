(* code/switchtoc.sig *)

signature SWITCHTOC =
  sig

    structure Code	: SWITCH
    val emitModule	: TextIO.outstream * ((string * string option) * Code.module) -> unit
    val emitInterface	: TextIO.outstream * Code.module -> unit

  end (* signature SWITCHTOC *)
