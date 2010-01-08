(* config/109.25.2/sysdep.sml *)

structure SysDep : SYSDEP = struct

    exception Interrupt

    (* This function applies operation to argument. If it handles an interrupt
     * signal (Control-C), it raises the exception Interrupt. Example:
     * (handleInterrupt(foo,x)) handle Interrupt => print "Bang!\n"
     *)
  
    fun handleInterrupt(operation : 'a -> unit, argument : 'a) =
        operation argument
      (*
      let exception Done
	  val oldHandler = Signals.inqHandler(Signals.sigINT)
	  fun resetHandler() =
	    Signals.setHandler(Signals.sigINT, oldHandler)
      in (SMLofNJ.Cont.callcc(fn k =>
		(Signals.setHandler(Signals.sigINT, Signals.HANDLER(fn _ => k));
		 operation argument;
		 raise Done));
	  raise Interrupt)
	 handle Done => (resetHandler(); ())
	      | exn  => (resetHandler(); raise exn)
      end
    *)
    
    (* val exportFn = SMLofNJ.exportFn *)

    local
      fun f([]) = []
	| f([x]) = [Int.toString x]
	| f(x::xs) = Int.toString x :: "." :: f xs
    in
      val smlVersion = "MLton or SML.NET or SMLNJ" 
      (* String.concat("SML/NJ":: f(#version_id CompilerVersion.version) *)
    end

  end (* structure SysDep *)
