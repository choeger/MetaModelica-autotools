(* interp/interp.sig *)

signature INTERP =
  sig

    structure Absyn : ABSYN
    val run : Absyn.module list * string list -> unit

  end (* signature INTERP *)
