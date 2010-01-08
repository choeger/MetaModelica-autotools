(* config/sysdep.sig *)

signature SYSDEP =
  sig

    exception Interrupt
    val handleInterrupt	: ('a -> unit) * 'a -> unit
    (* val exportFn	: string * (string * string list -> OS.Process.status) -> unit *)
    val smlVersion	: string

  end (* signature SYSDEP *)
