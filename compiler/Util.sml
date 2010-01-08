(* util/util.sml *)

structure Util : UTIL =
  struct

    fun sayErr s = (TextIO.output(TextIO.stdErr, s); TextIO.flushOut(TextIO.stdErr))
    
    fun outStdErr s = (TextIO.output(TextIO.stdErr, s); TextIO.flushOut(TextIO.stdErr))

    exception Bug
    fun bug s =
      (sayErr "Internal error: "; sayErr s; sayErr "\n"; raise Bug)

    exception Error
    fun error s =
      (sayErr "Error: "; sayErr s; sayErr "\n"; raise Error)
      
    fun warn s =
      (sayErr "Warning: "; sayErr s; sayErr "\n")      

    val tick =
      let val cnt = ref 99
      in
		fn() => let val i = !cnt + 1 in cnt := i; i end
      end

    fun member(x, []) = false
      | member(x, y::ys) = (x = y) orelse member(x, ys)

    fun maybeCons(x, xs) = if member(x, xs) then xs else x::xs

  end (* structure Util *)
