(* config/start.sml *)

functor StartFn(structure SysDep  : SYSDEP) : START =
  struct
    fun sayErr msg = TextIO.output(TextIO.stdErr, msg)
    fun start main (arg0,argv) =
      (
	  (* for profiling enable this and comment the next line after this comment
	  SMLofNJ.Internals.ProfControl.profileOn();
      SysDep.handleInterrupt(main, argv);	  
	  SMLofNJ.Internals.ProfControl.profileOff();
	  print "\n---- Timing report start ----\n";
	  Profile.report (TextIO.stdOut);
	  print "\n---- Timing report end   ----\n"; 
	  *)
      SysDep.handleInterrupt(main, argv);	  
      OS.Process.exit OS.Process.success
      )
      handle 
			SysDep.Interrupt => (sayErr "Interrupt\n"; OS.Process.exit OS.Process.failure)
	   |	exn => (sayErr "Error: "; sayErr(General.exnMessage exn);
		     sayErr "\n"; OS.Process.exit OS.Process.failure)
  end (* functor StartFn *)

