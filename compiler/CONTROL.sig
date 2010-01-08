(* main/control.sml *)

signature CONTROL =
  sig
  				
    type serializationInfo = 
             {
				file:    string, (* which filename was serialized *)
                date:    string, (* date as string when it was serialized *)
                version: int     (* version of the serialized file *) 
             }
	
	datatype filetype = UNKNOWN_FILE 
					  | RML_FILE 
					  | MO_FILE 
					  | RML_AST_FILE
					  | RML_FOL_FILE
					  | RML_CPS_FILE
					  | RML_RDB_FILE
					  | INTERFACE_FILE
					  | SERIALIZATION_FILE 
					  
    (* version of the serialized files. if we change their format we can discard them easy *)
	val serializationFileVersion: int
	val getSerializationInfo: string -> serializationInfo
	
    (* flag to write the symboltable or not*)
    val emitDebug : bool ref
    (* flag to emit the program database or not *)
    val emitRdb   : bool ref
    (* flag to emit the AST or not *)
    val emitAst   : bool ref
    (* flag to emit the FOL form or not *)
    val emitFol   : bool ref
    (* flag to emit the CPS form or not *)
    val emitCps   : bool ref

    (* flag that specifies if only typecheck should be performed, no codegen *)
    val onlyTypeCheck : bool ref
    
    (* options for ReorderParse *)
    val doReorder: bool ref

    (* options for TypeCheck / StatElab *)
    val allowImplicitLet: bool ref

    (* options for FOLToCPS *)
    val warnNonExhaustive: bool ref
    val printDFAStatistics: bool ref    

    (* options for CodeToC *)
    val switchRewriteThreshold: int ref

    (* should we do the debugging instrumentation? *)
    val doDebug: bool ref

    (* should we do a trace *)
    val doTrace: bool ref
    
    (* the program database should be qualified? *)
    val qualifiedRdb: bool ref

	(* should we only dump the program database *)
    val rdbOnly: bool ref
    
    (* should we fix the java names? *)
    val fixJavaNames : bool ref
    
    (* should we dump the interface? *)
    val dumpInterface : bool ref
    
    (* should we dump the dependency? *)
    val dumpDepends : bool ref
    
    (* what are we currently compiling? *)
    val currentlyCompiling : filetype ref
    
	(* this one helps in selecting messages depending on what we are currently compiling *)
	val selectCompilerMessage: string * string -> string    
    
    val fileBase:   string -> string
    val fileType:   string -> filetype
	val getFileExt: filetype -> string
    val pathSplit:  string -> (string * string option)
	val fileExists: string -> bool    
    val fileNewer: string * string -> bool (* see if file1 is newer than file2 *)
	val getFileName: string * filetype -> string 
    val fileCheckSerializationInfo: string -> bool
	val isSerializedFileValid: string -> bool
	val isInterfaceFileValid:  string -> bool
	val joinBaseExt: string * string option -> string
	
    (* datatype for the result if is ok or if is error *)
    datatype 'a outcome = OK of 'a | ERR of exn
    
    (* function to write files with error handling *)
    val withOutputOption: (TextIO.outstream option * 'a -> 'b) -> 'a -> string -> 'b
    (* function to write files with error handling *)
    val withOutput      : (TextIO.outstream * 'a -> 'b) -> 'a -> string -> 'b     
    (* function to write files with error handling *)
    val withOutputStream: ('a * 'b -> 'c) -> 'b -> 'a -> 'c
    (* function to write files with error handling *)
    val withOut         : (TextIO.outstream * 'a -> 'b) -> 'a -> 'b

    (* print import loadinging order  *)
    val importLoadOrder : bool ref

    val idirs : string list ref
	
  end (* signature CONTROL *)

