(* config/start.sml *)

signature START =
  sig
    val start : (string list -> unit) -> string * string list -> OS.Process.status
  end (* signature START *)

