(* lexer/lexarg.sig *)

signature LEXARG =
  sig

    structure Source:  SOURCE    
    structure StrDict : STR_DICT
            
    type poz		 (* char pozition + start line + end line *)
    type restriction
    type visibility
    
    type lexarg
    val new	         : string * TextIO.instream -> lexarg * (int -> string)
    val newLine	     : lexarg * poz -> unit
    val newTab	     : lexarg * int ref * poz -> unit
    val fixColumn    : lexarg * int -> unit
    val leftPos	     : lexarg -> poz ref
    val strList	     : lexarg -> string list ref
    val comLev	     : lexarg -> int ref
    val eofFlag	     : lexarg -> bool ref
    val readPos	     : lexarg -> int
    val seenErr	     : lexarg -> bool
    val currLine     : lexarg -> int ref
    val currColumn   : lexarg -> int ref
    val currVisibility: lexarg -> visibility ref
    
    val error2	     : lexarg -> string * poz * poz -> unit
    val warn	     : lexarg -> string * poz * poz -> unit
    val source	     : lexarg -> Source.source
    val getLoc	     : lexarg -> int * int -> {fileName:string, sline:int, scolumn:int, eline:int, ecolumn:int}
    val getPos       : lexarg * int -> int * int
	
    val addExternal    : lexarg * string * poz * poz -> unit 
    val addImport      : lexarg * string * poz * poz * visibility -> unit 
    val addRestriction : lexarg * string * restriction * visibility -> unit
    
    val addUndefImport      : lexarg * string * poz * poz  -> unit 
    val addUndefRestriction : lexarg * string * restriction -> unit
    val fixUndefImports     : lexarg * visibility -> unit
    val fixUndefRestrictions : lexarg * visibility -> unit
    
    val getImports      : lexarg -> (poz * poz * visibility) StrDict.dict
    val getExternals    : lexarg -> (poz * poz) StrDict.dict
    val getRestrictions : lexarg -> (restriction * visibility) StrDict.dict
        
  end (* signature LEXARG *)
