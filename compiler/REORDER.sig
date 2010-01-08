(* reorder/reorder.sig *)

signature REORDER =
  sig

    structure Absyn: ABSYN
    val reorderInterface : Absyn.interface -> Absyn.interface
    val reorderModule    : Absyn.module    -> Absyn.module

  end (* signature REORDER *)
