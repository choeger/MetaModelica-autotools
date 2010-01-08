(* parser/parse.sig *)

signature PARSE =
  sig

    structure Absyn	: ABSYN

    val parseModule	    : string -> Absyn.module
    val parseInterface	: string -> Absyn.module
    
	val mo2rmlmodule		  :  Absyn.modelica -> Absyn.module
	val mo2rmlinterface		  :  Absyn.modelica -> Absyn.module
	val mo2imports            :  Absyn.modelica -> (string list * Absyn.modelica)
	val mo2recordconstructors :  Absyn.modelica -> Absyn.Package list
  end (* signature PARSE *)
