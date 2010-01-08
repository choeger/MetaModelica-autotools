(* absyntofol/absyntofol.sig *)

signature ABSYNTOFOL =
  sig
  
    structure Absyn	: ABSYN
    structure FOL	: FOL

    val translate	: Absyn.module -> FOL.module

  end (* signature ABSYNTOFOL *)
