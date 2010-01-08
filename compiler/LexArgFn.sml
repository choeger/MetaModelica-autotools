(* lexer/lexarg.sml *)

functor LexArgFn(
	structure Source  : SOURCE
	structure Cache : CACHE) : LEXARG =
  struct

    structure Source  = Source
    structure StrDict = Cache.StrDict
     
    type restriction = Cache.restriction
    type visibility  = Cache.visibility
          
    type poz = Cache.poz (* char pozition + line + column *)
    datatype lexarg
      = A of 
      {	
        sourcemap   : Source.ArraySourceMap.sourcemap ref,
		thisLine	: int ref,
		leftPos		: poz ref,
		strList		: string list ref,
		comLev		: int ref,
		eofFlag		: bool ref,
		errFlag		: bool ref,
		readPos		: poz ref,
		currLine    : int ref, 
		currColumn  : int ref, 
		currVisibility: visibility ref,
		imports     : (poz * poz * visibility) StrDict.dict ref,
		externals   : (poz * poz) StrDict.dict ref,
		restrictions: (restriction*visibility) StrDict.dict ref,
		i_queue: (poz * poz * string) list ref,
		r_queue: (restriction * string) list ref
	  }

	fun fixPos((poz, x, y), add) = (poz+add, x, y)

    fun new(fileName, is) =
      let val readPos = ref (2, 0, 0) (*XXX: ML-Lex*)
	  fun yyinput n =
	    let val string = TextIO.inputN(is, n)
		val _ = readPos := fixPos(!readPos, String.size string)
	    in
	      string
	    end
	  val lexarg =
	    A{sourcemap = ref(Source.ArraySourceMap.new(fileName,Source.getCurrentDate())),      
	      thisLine = ref 2,
	      leftPos =  ref (0,0,0),
	      strList = ref [],
	      comLev = ref 0,
	      eofFlag = ref false,
	      errFlag = ref false,
	      readPos = readPos,
	      currLine   = ref 1,
	      currColumn = ref 0,	      
	      currVisibility = ref Cache.PUBLIC,
		  imports   = ref StrDict.empty,
		  externals = ref StrDict.empty,
		  restrictions = ref StrDict.empty,
		  i_queue = ref [],
		  r_queue = ref []
	      }
      in
		(lexarg, yyinput)
      end

    fun newLine(A{sourcemap,thisLine,currLine,currColumn,...}, (poz,l,c)) =
      (Source.ArraySourceMap.newline(!sourcemap, poz); 
		thisLine   := poz + 1;
		currLine   := !currLine + 1;
		currColumn := 0)

    fun newTab(A{thisLine,readPos,currColumn,...}, yygone, (yypoz,l,c)) =
      let val lpoz = yypoz - !thisLine
      (* adrpo changed 2005-01-18: TAB SHOULD BE 1 CHARACTER *)
	  val incr = 0 (* 7 - Int.rem(lpoz, 8) *)
      in
		readPos    := fixPos(!readPos, incr);
		currColumn := !currColumn + incr;
		yygone  := !yygone + incr
      end
      
    fun fixColumn(A{thisLine,readPos,currColumn,...}, yypoz) =
    (
      currColumn := yypoz - !thisLine
    )

    fun leftPos(A{leftPos,...}) = leftPos
    fun strList(A{strList,...}) = strList
    fun comLev(A{comLev,...})   = comLev
    fun eofFlag(A{eofFlag,...}) = eofFlag
    fun readPos(A{readPos=ref(rp,_,_),...}) = rp
    fun seenErr(A{errFlag,...}) = !errFlag
    fun currLine(A{currLine,...})     = currLine
    fun currColumn(A{currColumn,...}) = currColumn
    fun currVisibility(A{currVisibility,...}) = currVisibility    
    fun getPos(A{thisLine,readPos,currLine,...}, poz) = (!currLine, poz - !thisLine)

    fun source(A{sourcemap,...}) = Source.SOURCE(sourcemap)

	exception ParseError
	
    fun error2 (lexarg as A{errFlag,...}) (msg,(left,_,_),(right,_,_)) =
      (errFlag := true;
       Source.sayMsg (source lexarg) ("Error: "^msg, left, right);
       raise ParseError)

    fun warn (lexarg as A{errFlag,...}) (msg,(left,_,_),(right,_,_)) =
      (Source.sayMsg (source lexarg) ("Warning: "^msg, left, right))
       
    fun getLoc lexarg (left, right) = Source.getLoc(source lexarg, left, right)
    (* was before:
	  let val fileName = Source.getFileName(source lexarg)
	  in
		{
			fileName = fileName,
			sline    = sl, 
			scolumn  = sc, 
			eline    = el, 
			ecolumn  = ec
		}
	  end
	*)

    fun addImport (lexarg as A{imports,...}, import, left, right, visibility) =
    let val there = StrDict.find(!imports, import)
    in    
		case there of
			NONE => imports := StrDict.insert(!imports, import, (left, right, visibility)) 
		|	SOME((_,_,x)) =>
				case (x, visibility) of
					(Cache.PUBLIC,Cache.PROTECTED)
					=> imports := StrDict.insert(!imports, import, (left, right, Cache.BOTH)) 
				|	(Cache.PROTECTED,Cache.PUBLIC)		 
					=> imports := StrDict.insert(!imports, import, (left, right, Cache.BOTH)) 
				|	(_,_) (* leave it like it is *)  => ()
				(*	=> imports := StrDict.insert(!imports, import, (left, right, visibility))*) 
		end   

    fun addExternal (lexarg as A{externals,...}, external, left, right) =
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

    fun addUndefImport(lexarg as A{i_queue,...}, str, left, right) = 
			i_queue := (left,right,str)::(!i_queue)
    
    fun addUndefRestriction(lexarg as A{r_queue,...},str,restriction) =
			r_queue := (restriction,str)::(!r_queue)
    
    fun fixUndefImports(lexarg as A{i_queue,...}, visibility) =
    let fun fixImport(left,right,str) = addImport(lexarg, str, left, right, visibility) 
    in
			map fixImport (!i_queue); i_queue := []
    end
    
    fun fixUndefRestrictions(lexarg as A{r_queue,...},visibility) =
    let fun fixRestriction(restr,str) = addRestriction(lexarg, str, restr, visibility) 
    in
			map fixRestriction (!r_queue); r_queue := []
    end

    fun getImports      (lexarg as A{imports,...})      = !imports 
    fun getExternals    (lexarg as A{externals,...})    = !externals
    fun getRestrictions (lexarg as A{restrictions,...}) = !restrictions
        
  end (* functor LexArgFn *)
