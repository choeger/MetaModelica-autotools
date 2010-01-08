(* code/code_fvars.sig *)

signature CODE_FVARS =
  sig

    structure Code	: CODE 
    val update		: Code.module -> unit

  end (* signature CODE_FVARS *)
