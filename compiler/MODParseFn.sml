(* MODParseFn.sml *)

functor MODParseFn(structure Absyn : ABSYN
		structure Cache : CACHE
		structure LexArg : LEXARG where type poz = Cache.poz
		structure MODParser : ARG_PARSER
				      where type arg = LexArg.lexarg
					  and type pos = Cache.poz
		structure Tokens : MOD_TOKENS
		structure Util : UTIL
		sharing type Tokens.token = MODParser.Token.token
		sharing type Tokens.svalue = MODParser.svalue
		sharing type MODParser.result = Absyn.modelica = Cache.Absyn.modelica		
		sharing type MODParser.lexarg = LexArg.lexarg
		sharing type Cache.Absyn.Source.source = LexArg.Source.source
		sharing type Cache.StrDict.dict = LexArg.StrDict.dict
		sharing type Cache.restriction = LexArg.restriction
		sharing type Cache.visibility = LexArg.visibility
		  ) : MOD_PARSE =
  struct
  
    structure Absyn = Cache.Absyn
    type repository = Cache.repository
    
	val debugFlag = false
	fun debug s = if (debugFlag) then Util.outStdErr("MODParseFn."^s) else ()	
	
	(* If file found in search path(s), return file path otherwise return file *)
	fun prepathFile(file, paths) =
	  case paths of
	    nil => file
	    | _ => let val fp = OS.Path.joinDirFile{dir = (hd paths), file = file}
	           in
	             if Control.fileExists fp then fp
	             else prepathFile(file, tl paths)
	           end
		
    fun parse(startToken, file, repository, isInterface) =
	let val f = if Control.fileExists(file) then 
	              file (* If the file is in current dir, use it *)
	            else  (* otherwise prepend includ path and check *)
	              prepathFile(file, rev (!Control.idirs))
	    val _ = debug("\nMODPraseFn.parse File: "^f^"\n")
	    val is = TextIO.openIn f
	in
	(let val (lexarg, inputf) = LexArg.new(file, is)
	     val pos = (2,0,0)	(*XXX: ML-Lex*)
	     val lexer = MODParser.makeLexer inputf lexarg
	     val lexer = MODParser.Stream.cons(startToken(pos,pos), lexer)
	     val (result,_) = 
			MODParser.parse(
				0,
				lexer,
				LexArg.error2 lexarg,
				lexarg)
	 in
	   if LexArg.seenErr lexarg then raise MODParser.ParseError else ();
	   TextIO.closeIn is;
	   let val Absyn.PROGRAM(
				 classes,
				 within, 
				 Absyn.INTERFACE({modid,specs,...}, infoI), 
				 infoM) = result
	       val interface = Absyn.INTERFACE({modid=modid,specs=specs,
					       source=LexArg.source lexarg}, 
					       infoI)				 
		   val program = Absyn.MOD_FILE(file, Absyn.PROGRAM(classes, within, interface, infoM))
	   in
	   Cache.insert(
			repository, 
			Cache.modCache,
			file,
			Cache.makeEntry(
			let val fileInfo =
				Cache.makeFileInfo(
					program,
					LexArg.getImports(lexarg),
					LexArg.getExternals(lexarg),
					LexArg.getRestrictions(lexarg),
					ref false,(* reordered *)
					ref false (* elaborated *)
					)
			in
				if (isInterface)
				then (modid, NONE, SOME(fileInfo))
				else (modid, SOME(fileInfo), NONE)
			end
			));
		program
	   end
	end) handle e => (TextIO.closeIn is; raise e)
    end


    fun parseModule   (file, repository) = 
    let val start = Time.now()
		val _ = debug("parseModule: "^file^" -> ")
		val result = parse(Tokens.START_MODELICA, file, repository, false)
		val stop = Time.now()
		val interval = Time.- (stop, start)
		val _ = debug("("^(Time.fmt 5 interval)^")\n")
	in
		result
	end
    fun parseInterface(file, repository) =
    let val start = Time.now()
		val _ = debug("parseInterface: "^file^" -> ")
		val result = parse(Tokens.START_MODELICA, file, repository, false)
		val stop = Time.now()
		val interval = Time.- (stop, start)
		val _ = debug("("^(Time.fmt 5 interval)^")\n")
	in
		result
	end
    	 
  end (* functor MODParseFn *)
