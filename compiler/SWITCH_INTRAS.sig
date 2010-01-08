(* code/switch_intras.sig *)

signature SWITCH_INTRAS =
  sig

    structure Code	: SWITCH 
    val labdefIntras : (Code.labdef * Code.gvar_name list) -> Code.gvar_name list

  end (* SWITCH_INTRAS *)
