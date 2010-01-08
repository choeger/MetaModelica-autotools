(* util/util.sig *)

signature UTIL =
  sig

    val bug			: string -> 'a
    val error		: string -> 'a
    val warn		: string -> unit
    
    val outStdErr	: string -> unit    

    val tick		: unit -> int

    val member		: ''a * ''a list -> bool
    val maybeCons	: ''a * ''a list -> ''a list

  end (* signature UTIL *)
