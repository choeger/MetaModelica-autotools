(* cpstocode/cpstocode.sig *)

signature CPSTOCODE =
  sig

    structure CPS	: CPS
    structure Code	: CODE
    val translate	: CPS.module -> Code.module

  end (* signature CPSTOCODE *)
