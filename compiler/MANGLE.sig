(* code/mangle.sig *)

signature MANGLE =
  sig

    datatype name	= NAME of string
    val encode		: string -> name
    val decode		: string -> string

  end (* signature MANGLE *)
