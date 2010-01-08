(* lexer/lexarg.sig *)

signature LEXARG_SIMPLE =
  sig
    
    structure StrDict : STR_DICT
	  type poz 
    type restriction
    type visibility
    
    type pos = int (* char position *)
    type lexarg
    val new	        : string * TextIO.instream -> lexarg * (int -> string)
    val newLine	    : lexarg * pos -> unit
    val newTab	    : lexarg * int ref * pos -> unit
    val leftPos	    : lexarg -> pos ref
    val strList	    : lexarg -> string list ref
    val comLev	    : lexarg -> pos ref
    val eofFlag	    : lexarg -> bool ref
    val readPos	    : lexarg -> pos
    val seenErr	    : lexarg -> bool
    val getFile     : lexarg -> string
    val currVisibility: lexarg -> visibility ref
    val isSerializationOf: lexarg -> string ref

    val errorMsg    : lexarg -> string * pos * pos -> unit

    val addImport      : lexarg * string * (poz * poz) * visibility -> unit 
    val addExternal    : lexarg * string * (poz * poz) -> unit 
    val addRestriction : lexarg * string * restriction * visibility -> unit
     
    val getImports      : lexarg -> (poz * poz * visibility) StrDict.dict
    val getExternals    : lexarg -> (poz * poz) StrDict.dict
    val getRestrictions : lexarg -> (restriction * visibility) StrDict.dict
        
  end (* signature LEXARG_SIMPLE *)
