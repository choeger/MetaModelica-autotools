(* lexer/lexarg.sml *)

functor LexArgSimpleFn(
	structure Cache: CACHE
	structure Util: UTIL
	) : LEXARG_SIMPLE =
  struct
     
    structure StrDict = Cache.StrDict
     
    type restriction = Cache.restriction
    type visibility  = Cache.visibility
    
		type poz = Cache.poz
     
    type pos = int (* char position *)
    datatype lexarg
      = A of {	
        fileName    : string,
        srzOfFile   : string ref,
		thisLine	: int ref,
		leftPos		: int ref,
		strList		: string list ref,
		comLev		: int ref,
		eofFlag		: bool ref,
		errFlag		: bool ref,
		readPos		: int ref,
		currLine    : int ref,
		currVisibility : visibility ref,
		imports     : (poz * poz * visibility) StrDict.dict ref,
		externals   : (poz * poz) StrDict.dict ref,
		restrictions: (restriction * visibility) StrDict.dict ref		
		}

    fun new(fileName, is) =
      let   val readPos = ref 2 (*XXX: ML-Lex*)
			fun yyinput n =
				let val string = TextIO.inputN(is, n)
				val _ = readPos := !readPos + String.size string
				in
				  string
				end
	  val lexarg =
	    A{fileName = fileName,
		  srzOfFile = ref "",
	      thisLine = ref 2,
	      leftPos =  ref 0,
	      strList = ref [],
	      comLev = ref 0,
	      eofFlag = ref false,
	      errFlag = ref false,
	      readPos = readPos,
	      currLine = ref 1,
	      currVisibility = ref Cache.PUBLIC,
		  imports   = ref StrDict.empty,
		  externals = ref StrDict.empty,
		  restrictions = ref StrDict.empty	      
	      }
      in
		(lexarg, yyinput)
      end

    fun newLine(A{thisLine,currLine,...}, pos) =
      ( 
        thisLine := pos + 1; 
        currLine := !currLine + 1 
      )

    fun newTab(A{thisLine,readPos,...}, yygone, yypos) =
      let val lpos = yypos - !thisLine
      (* adrpo changed 2005-01-18: TAB SHOULD BE 1 CHARACTER *)
	  val incr = 0 (* 7 - Int.rem(lpos, 8) *)
      in
		readPos := !readPos + incr;
		yygone  := !yygone + incr
      end

    fun leftPos(A{leftPos,...}) = leftPos
    fun strList(A{strList,...}) = strList
    fun comLev(A{comLev,...})   = comLev
    fun eofFlag(A{eofFlag,...}) = eofFlag
    fun readPos(A{readPos,...}) = !readPos
    fun seenErr(A{errFlag,...}) = !errFlag
    fun getFile(A{fileName,...}) = fileName    
    fun currVisibility(A{currVisibility,...}) = currVisibility    
    fun isSerializationOf(A{srzOfFile,...}) = srzOfFile 
    
	exception ParseError
	
    fun errorMsg (lexarg as A{fileName,errFlag,currLine,...}) (msg,_,_) =
      (errFlag := true;
       Util.outStdErr (fileName^":"^(Int.toString(!currLine))^" Error: "^msg^"\n");
       raise ParseError)
       
    fun addImport (lexarg as A{imports,...}, import, (left, right), visibility) =
    let val there = StrDict.find(!imports, import)
    in
		(*
		case (visibility) of 
			Cache.PROTECTED => print ("["^import^"]-PROTECTED\n")
		|	_ => print ("["^import^"]-PUBLIC\n");
		*)
		
		case there of
			NONE => imports := StrDict.insert(!imports, import, (left, right, visibility)) 
		|	SOME((_,_,x)) =>
				case (x, visibility) of
					(Cache.PUBLIC,Cache.PROTECTED)
					=> imports := StrDict.insert(!imports, import, (left, right, Cache.BOTH)) 
				|	(Cache.PROTECTED,Cache.PUBLIC)		 
					=> imports := StrDict.insert(!imports, import, (left, right, Cache.BOTH)) 
				|	(_,_) (* leave it like it is *) => ()
				(*	=> imports := StrDict.insert(!imports, import, (left, right, visibility)) *)
	end   


    fun addExternal (lexarg as A{externals,...}, external, (left, right)) =
      ( externals := StrDict.insert(!externals, external, (left, right)) )
    
    fun addRestriction (lexarg as A{restrictions,...}, key, restriction, visibility) =
    let val there = StrDict.find(!restrictions, key)
    in
		(*
		case (visibility) of 
			Cache.PROTECTED => print ("["^key^"]-PROTECTED\n")
		|	_ => print ("["^key^"]-PUBLIC\n");
		*)
    
		case there of
			NONE => restrictions := StrDict.insert(!restrictions,  key, (restriction, visibility))
		|	SOME((_,x)) =>
				case (x, visibility) of
					(Cache.PUBLIC,Cache.PROTECTED)
					=> restrictions := StrDict.insert(!restrictions,  key, (restriction, Cache.BOTH))
				|	(Cache.PROTECTED,Cache.PUBLIC)		 
					=> restrictions := StrDict.insert(!restrictions,  key, (restriction, Cache.BOTH))
				|	(_,_) (* leave it like it is *) => ()
				(*	=> restrictions := StrDict.insert(!restrictions,  key, (restriction, visibility)) *)
	end   

    fun getImports      (lexarg as A{imports,...})      = !imports 
    fun getExternals    (lexarg as A{externals,...})    = !externals
    fun getRestrictions (lexarg as A{restrictions,...}) = !restrictions

  end (* functor LexArgFn *)
