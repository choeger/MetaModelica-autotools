(* code/code_optim.sig *)

signature CODE_OPTIM =
  sig

    structure Code	: CODE
    val optimize	: Code.module -> Code.module

  end (* signature CODE_OPTIM *)
