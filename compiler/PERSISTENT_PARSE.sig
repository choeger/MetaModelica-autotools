(* parser/persistent_parse.sig *)

signature PERSISTENT_PARSE =
  sig

    structure Absyn	: ABSYN
    type repository

    val parseModule	            : string * repository -> Absyn.program
    val parseInterface	        : string * repository -> Absyn.program
    val parseSerializationInfo	: string -> {file: string, date: string, version: int}
    
  end (* signature PERSISTENT_PARSE *)
