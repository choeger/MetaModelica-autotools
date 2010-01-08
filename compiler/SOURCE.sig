(* util/source.sig *)

signature SOURCE =
  sig

    structure ArraySourceMap: ARRAY_SOURCE_MAP
        
    datatype source
      = SOURCE of ArraySourceMap.sourcemap ref (* source map *)

    val dummy	: source
    
    val sayMsg	: source -> string * int * int -> unit

	val lookup : ArraySourceMap.sourcemap * int -> {line:int, column:int} 
	
	val getLoc : source * int * int -> {fileName: string, sline:int, scolumn:int, eline:int, ecolumn:int}	
	
	val getCurrentDate      : unit   -> string
	val getFileName         : source -> string
    val getLines            : source -> int list 
    val getCurrentLine      : source -> int 
    val getSerializationDate: source -> string
    
    val getSource:  string * string * int list * int -> source
	
  end (* signature SOURCE *)
