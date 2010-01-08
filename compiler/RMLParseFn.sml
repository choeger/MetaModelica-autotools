(* RMLParseFn.sml *)

functor RMLParseFn(structure Absyn : ABSYN
		structure Cache : CACHE
		structure LexArg : LEXARG where type poz = Cache.poz
		structure RMLParser : ARG_PARSER
				      where type arg = LexArg.lexarg
					  and type pos = Cache.poz
		structure Tokens : RML_TOKENS
		structure Control : CONTROL
		structure Reorder : REORDER				
		sharing type Tokens.token = RMLParser.Token.token
		sharing type Tokens.svalue = RMLParser.svalue
		sharing type RMLParser.result = Absyn.module = Cache.Absyn.module = Reorder.Absyn.module
		sharing type RMLParser.lexarg = LexArg.lexarg
		sharing type Cache.Absyn.Source.source = LexArg.Source.source
		sharing type Cache.StrDict.dict = LexArg.StrDict.dict
		sharing type Cache.restriction = LexArg.restriction
		sharing type Cache.visibility = LexArg.visibility
		  ) : RML_PARSE =
  struct
  
    structure Absyn = Cache.Absyn
    type repository = Cache.repository
    
	val debugFlag = false
	fun debug s = if (debugFlag) then Util.outStdErr("RMLParseFn."^s) else ()	

    fun parse(startToken, file, repository, isInterface) =
      let val is = TextIO.openIn file
      in
	(let val (lexarg, inputf) = LexArg.new(file, is)
	     val pos = (2,0,0)	(*XXX: ML-Lex*)
	     val lexer = RMLParser.makeLexer inputf lexarg
	     val lexer = RMLParser.Stream.cons(startToken(pos,pos), lexer)
	     val (result,_) = 
			RMLParser.parse(
				0,
				lexer,
				LexArg.error2 lexarg,
				lexarg)
	 in
	   if LexArg.seenErr lexarg then raise RMLParser.ParseError else ();
	   TextIO.closeIn is;
	   let val Absyn.MODULE(
					Absyn.INTERFACE({modid,specs,...}, infoI), decs, infoM) = result
	       val interface = Absyn.INTERFACE({modid=modid,specs=specs,
					       source=LexArg.source lexarg}, 
					       infoI)
		   val module = Absyn.MODULE(interface, decs, infoM)
		   val program = Absyn.RML_FILE(file, Reorder.reorderModule(module))
	   in
	   Cache.insert(
			repository, 
			Cache.rmlCache,
			file,
			Cache.makeEntry(
			let val fileInfo =
				Cache.makeFileInfo(
					program,
					LexArg.getImports(lexarg),
					LexArg.getExternals(lexarg),
					LexArg.getRestrictions(lexarg),
					ref true, (* reordered *)
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
		val _ = debug(".parseModule: "^file^" -> ")
		val result =     parse(Tokens.START_MODULE,    file, repository, false)
		val stop = Time.now()
		val interval = Time.- (stop, start)
		val _ = debug("("^(Time.fmt 5 interval)^")\n")
	in
		result
	end
	
    fun parseInterface(file, repository) = 
    let val start = Time.now()
		val _ = debug("parseInterface: "^file^" -> ")
		val result = parse(Tokens.START_INTERFACE, file, repository, true)
		val stop = Time.now()
		val interval =  Time.- (stop, start)
		val _ = debug("("^(Time.fmt 5 interval)^")\n")
	in
		result
	end
    
  end (* functor RMLParseFn *)
