(* lexer/lexutil.sig *)

signature LEXUTIL =
  sig

    val decint	: string -> int
    val hexint	: string -> int
    val icon	: string -> int option
    val rcon	: string -> real
    val ccon	: string -> char
    val scon	: string -> string

  end (* signature LEXUTIL *)
