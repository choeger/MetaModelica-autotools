(* FRONTEND.sig *)

signature FRONTEND =
  sig
  
  structure Absyn   : ABSYN
  structure StrDict : STR_DICT
   
  type repository
  
  val processFile    :  (string * string option) * repository -> Absyn.module option
  val processProgram : 	(string * string option) list *	repository -> Absyn.module list 
  
  end (* signature FRONTEND *)
