(* util/makestring.sig *)

signature MAKESTRING =
  sig

    val ccvt	: char -> string
    val icvt	: int -> string
    val rcvt	: real -> string
    val scvt	: string -> string

  end (* signature MAKESTRING *)
