(* MOTORML.sig *)

signature MOTORML =
  sig

    structure Absyn	: ABSYN
	type repository    
	val transformMOToRML :  Absyn.modelica * string list * repository -> Absyn.module
	
  end 

(* signature MOTORML *)
