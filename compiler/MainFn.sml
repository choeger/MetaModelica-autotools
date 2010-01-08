(* main/main.sml *)
(* adrpo
 * - added comments
 * - added calls to AST instrumentation
 *)

(* the main functor *)
functor MainFn(
	       structure Cache   : CACHE
           structure Version : VERSION
	       structure Util : UTIL
	       structure FrontEnd : FRONTEND where type repository = Cache.repository
	       structure AbsynToFOL : ABSYNTOFOL
	       structure FOLPrint : FOL_PRINT
	       structure FOLOptim : FOL_OPTIM
	       structure FOLToCPS : FOLTOCPS 
	       structure CPSOptim : CPS_OPTIM
	       structure CPSPrint : CPS_PRINT
	       structure CPSToSwitch : CPSTOCODE (* where type Code.gvar = Switch.gvar' *)
	       structure SwitchOptim : CODE_OPTIM 
	       structure CPSToPlain : CPSTOCODE (* where type Code.gvar = Plain.gvar' *)
	       structure PlainOptim : CODE_OPTIM
	       structure PlainToC : PLAINTOC
	       structure MaskToC : SWITCHTOC
	       structure DiffToC : SWITCHTOC
	       structure Interp : INTERP
	       structure CPSToSML : CPSTOSML
	       structure Control : CONTROL
           sharing type FrontEnd.Absyn.module = Interp.Absyn.module = AbsynToFOL.Absyn.module
	       sharing type AbsynToFOL.FOL.module = FOLPrint.FOL.module = 
	                    FOLOptim.FOL.module = FOLToCPS.FOL.module
	       
	       sharing type FOLToCPS.CPS.module = CPSOptim.CPS.module = CPSPrint.CPS.module =
		                CPSPrint.CPS.module = CPSToSwitch.CPS.module
	       sharing type CPSToSwitch.Code.module = SwitchOptim.Code.module = 
	                    MaskToC.Code.module = DiffToC.Code.module
	       sharing type CPSToPlain.Code.module = PlainOptim.Code.module = PlainToC.Code.module
	       
	       sharing type CPSPrint.CPS.module = CPSToPlain.CPS.module
	       sharing type CPSToSML.CPS.module = FOLToCPS.CPS.module
		 ) : MAIN =
  struct
		
    (* error printing *)
    fun sayErr msg = TextIO.output(TextIO.stdErr, msg)
        
    (* enumeration datatype of code generation schemes (backends) *)
    datatype cgscheme = PLAIN | MASK | DIFF | SML
    (* default code generation scheme is set to PLAIN *)
    val cgScheme = ref PLAIN
    (* flag to optimize FOL representation or not *)
    val optFol = ref true
    (* flag to optimize CPS representation or not *)
    val optCps = ref true
    (* flag to optimize Code representation or not *)
    val optCode = ref true
    
    structure StrDict = Cache.StrDict
    type repository = Cache.repository
    
    (* function that does the rml to sml translation *)
    fun doSml((prefix, ext), cpsModule) =
      (Control.withOutput CPSToSML.emitModule ((prefix, ext), cpsModule) (prefix ^ ".sml");
       Control.withOutput CPSToSML.emitInterface cpsModule (prefix ^ ".sig"))

    (* generates code according to Switch runtime *)
    fun doSwitch((prefix, ext), switchModule) =
      let val switchModule = 
		  if !optCode 
		  then SwitchOptim.optimize switchModule
		  else switchModule
	  val cFile = prefix ^ ".c" and hFile = prefix ^ ".h"
      in
		case !cgScheme of 
			DIFF =>
	      (Control.withOutput DiffToC.emitModule ((prefix, ext), switchModule) cFile;
	       Control.withOutput DiffToC.emitInterface switchModule hFile)
	    | _ =>
	      (Control.withOutput MaskToC.emitModule ((prefix, ext), switchModule) cFile;
	       Control.withOutput MaskToC.emitInterface switchModule hFile)
      end

    (* generates code according to Plain runtime *)
    fun doPlain((prefix, ext), plainModule) =
      let val plainModule = if !optCode then PlainOptim.optimize plainModule
			    else plainModule
      in
		Control.withOutput PlainToC.emitModule ((prefix, ext), plainModule) (prefix ^ ".c");
		Control.withOutput PlainToC.emitInterface plainModule (prefix ^ ".h")
      end
      
    (* dump the FOL representation *)
    fun folPrintOptim(folOs, folModule) =
      FOLOptim.optimize(SOME folOs, folModule)
            
    fun folPrint(folOs, folModule) =
		(FOLPrint.printModule(folOs, folModule); folModule)
      
    (* generate the FOL representation *)
    fun doFol((prefix, ext), folModule) =
      if !Control.emitFol 
      then if !optFol then
			Control.withOutput folPrintOptim 
				(Control.withOutput folPrint folModule (prefix ^ ".fol")) 
				(prefix ^ ".optim.fol")
		   else
			Control.withOutput folPrint folModule (prefix ^ ".fol")
      else if !optFol then 
			FOLOptim.optimize(NONE, folModule)
		   else 
			folModule

    (* dump the CPS representation *)
    fun cpsPrint(cpsOs, cpsModule) =
      if !optCps 
      then (CPSPrint.printModule(cpsOs, cpsModule);	CPSOptim.optimize(SOME cpsOs, cpsModule))
      else (CPSPrint.printModule(cpsOs, cpsModule); cpsModule)
    
    (* generate the CPS representation *)
    fun doCps((prefix, ext), cpsModule) =
      if !Control.emitCps 
      then Control.withOutput cpsPrint cpsModule (prefix ^ ".cps")
      else  if !optCps 
			then CPSOptim.optimize(NONE, cpsModule)
			else cpsModule
           
    (* function that translates a RML file into C representation (actual compilation) *)
    fun translate ( (prefix, ext), repository )=
      let	val fileName = OS.Path.joinBaseExt {base = prefix, ext = ext}
			val repository = Cache.new(
									Cache.new(
										Cache.new(
											ref StrDict.empty, Cache.rmlCache), 
										Cache.modCache), 
								   Cache.srzCache)			 
			val astModule = FrontEnd.processFile((prefix,ext), repository)
	  in	  		
		case astModule of
			SOME(astModule) =>
				let (* print the FOL ast if required by -Efol *)
					val folModule = doFol((prefix, ext), AbsynToFOL.translate astModule)
					(* print the CPS ast if required by -Ecps *)
					val cpsModule = doCps((prefix, ext), FOLToCPS.translate folModule)
				in
					case !cgScheme (* what backend should we use for codegen *)
					of PLAIN => doPlain((prefix, ext), CPSToPlain.translate cpsModule)
					|  MASK  => doSwitch((prefix, ext), CPSToSwitch.translate cpsModule)
					|  DIFF  => doSwitch((prefix, ext), CPSToSwitch.translate cpsModule)
					|  SML   => doSml((prefix, ext), cpsModule)
				end
		|	NONE => ()
      end

	fun helpBuiltin() =
	(
       sayErr "defined builtin relations/functions are:\n";	
       sayErr "print(string) => ()\n";
       sayErr "[more to come here later; i was lazy]\n"
    )

	fun help() =
	(
       sayErr "usage: rml [options] file1.(rml|mo) ... fileN.(rml|mo)\n";
       sayErr "NOTE: you cannot mix .rml files with .mo files when given multiple files\n";       
       sayErr "valid options are:\n";
       sayErr "--\n";
       sayErr "-E{no-}ast\n  do {not} dump the ast representation to file.ast; default to 'no'\n";
       sayErr "-E{no-}cps\n  do {not} dump the cps representation to file.cps; default to 'no'\n";
       sayErr "-E{no-}fol\n  do {not} dump the fol representation to file.fol and file.optim.fol; default to 'no'\n";
       sayErr "-E{no-}rdb\n  do {not} dump the program database representation to file.rdb; default to 'no'; activated when -fdebug is used\n";
       sayErr "-E{plain,diff,mask,sml}\n  generates code for the specified runtime; default to 'plain'\n";
       sayErr "-f{no-}implicit-let\n  should implicit let be allowed? x = y instead of let x = y; x shoud be unbound; default to 'no'\n";
       sayErr "-f{no-}reorder\n  should a reorder phase be applied after parsing? default to 'yes'\n";
       sayErr "-fswitch-rewrite-threshold=<integer>\n  if there are less than <integer> cases in a switch rewrite to if; default to '3'\n";
       sayErr "-f{no-}typecheck-only\n  only perform typecheck and do not generate code; default to 'no'\n";
       sayErr "-f{no-}trace\n  generate code to print all calls to the standard error; NOTE: could be very large; default to 'no'\n";
       sayErr "-f{no-}debug\n  generate debugging code\n";
       sayErr "-f{no-}qualified-rdb\n  do {not} dump the qualified program database representation to file.rdb; default to 'no'\n";
       sayErr "-f{no-}rdb-only\n  just dump the program database to file.rdb; also -fqualified-rdb and -ftypecheck-only are activated; default to 'no'\n";
       sayErr "-f{no-}dfa-statistics\n  print the pattern-match generated DFA statistics; default to 'no'\n";
       sayErr "-W{no-}non-exhaustive\n  warn of non-exhaustive pattern matching; default to 'no'\n";
       sayErr "-fdump-interface\n  dump the interface to the standard output\n";
       sayErr "-fdump-depends\n  dump the dependencies to the standard output\n";
       sayErr "-O\n  enable all optimizations; default to 'yes'\n";
       sayErr "-O0\n  disable all optimizations; default to 'no'\n";
       sayErr "-O{,no-}code\n  enable the optimization of code; default to 'yes'\n";
       sayErr "-O{,no-}cps\n  enable the optimization of cps; default to 'yes'\n";
       sayErr "-O{,no-}fol\n  enable the optimization of fol; default to 'yes'\n";
       sayErr "-f{no-}import-load-order\n  hide/show the import load order when compiling; default to 'no'\n";
       sayErr "-i\n  generate a SML interpreter\n";
       sayErr "-v\n  print the version and exit\n";
       sayErr "-builtin\n  print the defined builtin relations/functions and exit\n";
       sayErr "-help|--help|-h\n  print the help and exit\n";
       sayErr "-Idir\n  add directory dir to the list of directories to be searched for source files\n"
    )

    fun usage msg = (* check the compiler line arguments (parameters) *)
      (sayErr(msg);
       help();
       Util.error "Usage")
    
    (* function that outputs the version of the compiler *)
    fun version() = 
      (sayErr "rml+mmc compiler version ";
       sayErr Version.versionNumber;
       sayErr " built ";
       sayErr Version.builtDate;
       sayErr " using ";
       sayErr Version.builtUsing;
       sayErr "\n")

    (* function that eats the compiler options and sets the flags *)
    fun option arg = 
      case arg
	of "-East"     => Control.emitAst := true
	 | "-Eno-ast"  => Control.emitAst := false
	 | "-Efol"     => Control.emitFol := true
	 | "-Eno-fol"  => Control.emitFol := false
	 | "-Ecps"     => Control.emitCps := true
	 | "-Eno-cps"  => Control.emitCps := false
	 | "-Erdb"     => Control.emitRdb := true
	 | "-Eno-rdb"  => Control.emitRdb := false
	 | "-Ofol"     => optFol := true
	 | "-Ono-fol"  => optFol := false
	 | "-Ocps"     => optCps := true
	 | "-Ono-cps"  => optCps := false
	 | "-Ocode"    => optCode := true
	 | "-Ono-code" => optCode := false
	 | "-O"        => (optFol := true; optCps := true; optCode := true)
	 | "-O0"       => (optFol := false; optCps := false; optCode := false)
	 | "-Eplain"   => cgScheme := PLAIN
	 | "-Emask"    => cgScheme := MASK
	 | "-Ediff"    => cgScheme := DIFF
	 | "-Esml"     => cgScheme := SML
	 | "-fimplicit-let"      => Control.allowImplicitLet := true
	 | "-fno-implicit-let"   => Control.allowImplicitLet := false
	 | "-freorder"           => Control.doReorder := true
	 | "-fno-reorder"        => Control.doReorder := false
	 | "-ftypecheck-only"    => Control.onlyTypeCheck := true
	 | "-fno-typecheck-only" => Control.onlyTypeCheck := false
	 | "-fdebug" => 
	 (
		Control.doDebug := true; 
		Control.emitRdb := true; 
		Control.qualifiedRdb := true;
		(* no cps optimization *)
		optCps := false;
		(* disable code optimization *)
		optCode := false
	 )
	 | "-fno-debug"         => Control.doDebug := false
	 | "-ftrace"            => Control.doTrace := true
	 | "-fno-trace"         => Control.doTrace := false
	 | "-fqualified-rdb"    => Control.qualifiedRdb := true
	 | "-fno-qualified-rdb" => Control.qualifiedRdb := false
	 | "-frdb-only" => 
		(
		 Control.emitRdb := true; 
		 Control.rdbOnly := true;
		 Control.qualifiedRdb := false; 
		 Control.onlyTypeCheck := true
		)
	 | "-fno-rdb-only" => ()	 
	 | "-Wnon-exhaustive"    => Control.warnNonExhaustive := true
	 | "-Wno-non-exhaustive" => Control.warnNonExhaustive := false
	 | "-fdfa-statistics"    => Control.printDFAStatistics := true
	 | "-fno-dfa-statistics" => Control.printDFAStatistics := false	 
	 | "-ffix-java-names" => 
		(
		  Control.fixJavaNames := true;
		  Util.warn("#include \"ExternalRMLDefines.h\" will be added to the  generated .c file!")
		)	 
	 | "-fdump-interface" => 
		(
		  Control.dumpInterface := true
		)
	 | "-fdump-depends" => 
		(
		  Control.dumpDepends := true
		  (*
		  Control.dumpInterface := true (* do not load additional files *)	
		  *)	  
		)		
     | "-fno-import-load-order" => (Util.warn("-fno-import-load-order activated."); Control.importLoadOrder := false)
     | "-fimport-load-order" => (Util.warn("-fimport-load-order activated."); Control.importLoadOrder := true)
     | "-v" => version()
	 | "-builtin" => helpBuiltin()
	 | "--help" => help()
	 | "-help"  => help()
	 | "-h"     => help()
	 | _ =>
		if String.isPrefix "-I" arg then 
		  Control.idirs := String.substring(arg, 2, String.size arg - 2)::(!Control.idirs)
		else
	    let val size = String.size arg
		val srtPfx = "-fswitch-rewrite-threshold="
		val srtPfxSize = String.size srtPfx
	    in
	      if size > srtPfxSize andalso
		 String.substring(arg,0,srtPfxSize) = srtPfx then
		case Int.fromString(String.substring(arg,srtPfxSize,size-srtPfxSize))
		  of SOME i => Control.switchRewriteThreshold := i
		   | NONE => usage("rml: invalid argument '" ^ arg ^ "'\n")
	      else usage("rml: invalid argument '" ^ arg ^ "'\n")
	    end

    (* function that processes a rml file (compilation) *)
    fun compiler (argv, repository) =
      let fun process arg =
	    if String.sub(arg, 0) = #"-" then option arg
	    else
	      case (Control.fileType arg) of 
				Control.RML_FILE => 
				(
				case (!Control.currentlyCompiling) of
					Control.RML_FILE => ()
				|	Control.UNKNOWN_FILE => ()
				|	_ => usage("rml: invalid argument '" ^ arg ^ "'. You cannot mix .rml and .mo files\n");
				Control.currentlyCompiling := Control.RML_FILE;
				translate (Control.pathSplit arg, repository)
				)
		  |		Control.MO_FILE  => 
				(
				case (!Control.currentlyCompiling) of
					Control.MO_FILE => ()
				|	Control.UNKNOWN_FILE => ()
				|	_ => usage("rml: invalid argument '" ^ arg ^ "'. You cannot mix .rml and .mo files\n");
				Control.currentlyCompiling := Control.MO_FILE;				
				translate (Control.pathSplit arg, repository)
				)
		  |		_ => usage("rml: invalid argument '" ^ arg ^ "'\n")
      in
		List.app process argv
      end

    (* function to run the interpreter *)
    fun run(prefixes, argv, repository) =
	  let val repository = Cache.new(
							Cache.new(
								Cache.new(
									ref StrDict.empty, Cache.rmlCache), 
								Cache.modCache), 
						   Cache.srzCache)
	  val modseq = FrontEnd.processProgram(prefixes, repository)
      in
	    Interp.run(modseq, argv)
      end

    (* the main loop of the interpreter *)
    fun interpreter (argv, repository) =
      let fun revRun([], _) = ()
	    | revRun(prefixes, argv) = run(rev prefixes, argv, repository)
	  fun loop([], prefixes) = revRun(prefixes, [])
	    | loop("--"::argv, prefixes) = revRun(prefixes, argv)
	    | loop(arg::argv, prefixes) =
		if String.sub(arg, 0) = #"-" then
		  (option arg; loop(argv, prefixes))
		else
		let val (base,ext) = Control.pathSplit(arg)
		in
	      case (Control.fileType arg) of 
				Control.RML_FILE => 
				(
				case (!Control.currentlyCompiling) of
					Control.RML_FILE => ()
				|	Control.UNKNOWN_FILE => ()
				|	_ => usage("rml: invalid argument '" ^ arg ^ "'. You cannot mix .rml and .mo files\n");
				Control.currentlyCompiling := Control.RML_FILE;
				loop(argv, (base,ext)::prefixes)
				)
		  |		Control.MO_FILE  => 
				(
				case (!Control.currentlyCompiling) of
					Control.MO_FILE => ()
				|	Control.UNKNOWN_FILE => ()
				|	_ => usage("rml: invalid argument '" ^ arg ^ "'. You cannot mix .rml and .mo files\n");
				Control.currentlyCompiling := Control.MO_FILE;
				loop(argv, (base,ext)::prefixes)
				)
		  |		_ => usage("rml: invalid argument '" ^ arg ^ "'\n")
	  end
      in
		loop(argv, [])
      end

    (* the main function *)
    fun main argv =
      let val repository = Cache.new(
							Cache.new(
								Cache.new(
									ref StrDict.empty, Cache.rmlCache), 
								Cache.modCache), 
						   Cache.srzCache)
		fun loop("-i"::argv) = interpreter (argv, repository)
	    | loop("-v"::argv) = (version(); loop argv)
	    | loop argv = compiler (argv, repository)
      in
	    loop argv
      end

  end (* functor MainFn *)
