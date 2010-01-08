(* code/plaintoc.sig *)

signature PLAINTOC =
  sig

    structure Code	: PLAIN
    val emitModule	: TextIO.outstream * ((string * string option) * Code.module) -> unit
    val emitInterface	: TextIO.outstream * Code.module -> unit

  end (* signature PLAINTOC *)
