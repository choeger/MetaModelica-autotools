(* main/main.sig *)

signature MAIN =
  sig

	type repository

    val translate	: (string * string option) * repository -> unit
    val run		    : (string * string option) list * string list * repository -> unit
    val main		: string list-> unit 

  end (* signature MAIN *)
