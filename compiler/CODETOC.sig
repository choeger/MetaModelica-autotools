(* code/codetoc.sig *)

signature CODETOC =
  sig

    structure Code	     : CODE 
    val setCurrentSource : Code.Source.source -> unit
    val prInt		     : TextIO.outstream -> int -> unit
    val prReal		     : TextIO.outstream -> real -> unit
    val prSCON		     : TextIO.outstream -> string -> unit
    val prGVar		     : TextIO.outstream -> Code.gvar -> unit
    val prLVar		     : TextIO.outstream -> Code.lvar -> unit
    val prLabel		     : TextIO.outstream -> Code.label -> unit
    val prLabelUnmangled : TextIO.outstream -> Code.label -> unit
    val prVal		     : TextIO.outstream -> Code.value -> unit
    val prCode		     : TextIO.outstream -> Code.code -> unit
    val prImpLab	     : TextIO.outstream -> Code.label -> unit
    val emitLitdefs	     : TextIO.outstream -> (Code.litname * Code.litdef) list -> unit
    val prValDec	     : TextIO.outstream -> Code.label -> unit
    val emitValues	     : TextIO.outstream -> (Code.label * Code.litref) list -> unit
    val prInitProc	     : (TextIO.outstream * string) -> (unit -> unit) -> Code.module -> unit
    val prInterface	     : TextIO.outstream -> Code.module -> unit

    val instrumentLabel  : TextIO.outstream -> Code.label -> unit
    val	instrumentCont   : TextIO.outstream -> string * Code.position * string * string -> unit
    val getLabelName	 : Code.label -> string

  end (* signature CODETOC *)
