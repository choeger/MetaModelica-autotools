(* Control.sml *)

structure Control: CONTROL =
  struct

    structure PERSISTENTParse = PERSISTENTParse
    structure Util = Util
    
	fun bug  s = Util.bug("Control."^s)
        
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
	val serializationFileVersion = 4
	fun getSerializationInfo(file) = PERSISTENTParse.parseSerializationInfo(file)
		
    (* flag to write the symboltable or not*)
    val emitDebug = ref false
    (* flag to emit the program database or not *)
    val emitRdb = ref false
    (* flag to emit the AST or not *)
    val emitAst = ref false
    (* flag to emit the FOL form or not *)
    val emitFol = ref false
    (* flag to emit the CPS form or not *)
    val emitCps = ref false

    (* flag that specifies if only typecheck should be performed, no codegen *)
    val onlyTypeCheck = ref false       

    (* options for ReorderParse *)
    val doReorder = ref true

    (* options for TypeCheck / StatElab *)
    val allowImplicitLet = ref false

    (* options for FOLToCPS *)
    val warnNonExhaustive  = ref false
    val printDFAStatistics = ref false   

    (* options for CodeToC *)
    val switchRewriteThreshold = ref 3

    (* options for Debbuging - added by adrpo@ida.liu.se 2002-06-14 *)
    (* should we do the debugging instrumentation? *)
    val doDebug = ref false;
    
    (* should we do a trace *)
    val doTrace = ref false; 
    
    (* the program database should be qualified? *)
    (* option for including moduleIdent.ident or just ident in the rdb file *)
    val qualifiedRdb = ref true
	(* should we only dump the program database *)
    val rdbOnly      = ref false
    
    (* should we fix the java names? *)
    val fixJavaNames = ref false
    
    (* should we dump the interface? *)
    val dumpInterface = ref false

    (* should we dump the dependency? *)
    val dumpDepends = ref false
    
	(* print import loadinging order  *)
    val importLoadOrder = ref false

    (* what are we currently compiling? *)    
    val currentlyCompiling = ref UNKNOWN_FILE

	(* list of search paths *)
	val idirs = ref [""]
    
	(* this one helps in selecting messages depending on what we are currently compiling *)
	fun selectCompilerMessage(strRML, strMMC) =
	(
	case !currentlyCompiling of
		RML_FILE => strRML
	|	MO_FILE => strMMC
	|	_ => bug("selectCompilerMessage: We don't know what file type are we compiling!")
	)
    
	fun fileBase (file) =
	  let val {base,ext} = OS.Path.splitBaseExt file
	  in
	    base
	  end

	fun fileType (file) =
	  let val {base,ext} = OS.Path.splitBaseExt file
	  in
		case ext
		  of SOME "rml"  => RML_FILE
		   | SOME "mo"   => MO_FILE
		   | SOME "ast"  => RML_AST_FILE
		   | SOME "fol"  => RML_FOL_FILE
		   | SOME "cps"  => RML_CPS_FILE
		   | SOME "rdb"  => RML_RDB_FILE
		   | SOME "sig"  => INTERFACE_FILE		    
		   | SOME "srz"  => SERIALIZATION_FILE
		   | _           => UNKNOWN_FILE
	  end
	  
  	fun getFileExt(RML_FILE)           = "rml"
  	|	getFileExt(MO_FILE)            = "mo"  
	|	getFileExt(RML_AST_FILE)       = "ast"
	|	getFileExt(RML_FOL_FILE)       = "fol"
	|	getFileExt(RML_CPS_FILE)       = "cps"
	|	getFileExt(RML_RDB_FILE)       = "rdb"
	|   getFileExt(INTERFACE_FILE)     = "sig"
	|	getFileExt(SERIALIZATION_FILE) = "srz"
	|	getFileExt(_)                  = "unknown"
	  
	fun pathSplit (file) =
	  let val {base,ext} = OS.Path.splitBaseExt file
	  in
	   (base, ext)
	  end
	  
    fun fileExists(file) = (* see if there exists a file *)
	if OS.FileSys.access(file, []) 
	then true
	else false
	    
    fun fileNewer(file1, file2) = (* see if file1 is newer than file2 *)
	let val timeFile1 = OS.FileSys.modTime file1
		val timeFile2 = OS.FileSys.modTime file2
	in
	  case Time.compare(timeFile1, timeFile2) of
		  GREATER => true
		| _ => false
	end
	    
	fun getFileName(file, ftype) = 
	let val (base,ext) = pathSplit file
	in
		base ^ "." ^ getFileExt(ftype)
	end

    fun fileCheckSerializationInfo(file) =
	let val serializationFile = getFileName(file, SERIALIZATION_FILE)
	in
		if fileExists(serializationFile)
		then let val {file=serializedFile,date=date_str,version=version} = getSerializationInfo(serializationFile)
			 in
				 if (serializedFile = file) andalso
				    (version = serializationFileVersion)
				 then true
				 else false 
			 end
		else false
	end
		
	fun isSerializedFileValid(file) = 
	let val serializationFile = getFileName(file, SERIALIZATION_FILE)
	in
	  if fileExists(file) andalso 
	     fileExists(serializationFile) andalso
	     fileNewer(serializationFile, file) andalso 
	     fileCheckSerializationInfo(file) 
	  then true
	  else false 
	end
	
    datatype 'a outcome = OK of 'a | ERR of exn
	(* this function reads the first line of the text file given *)
	fun readFirstLine(file) = 
	(
		let val is = TextIO.openIn file
				(* read the first line and see if the file is right! *)
			val outcome = (OK(TextIO.inputN(is,18+String.size(file)))) handle exn => ERR exn
      in
	    TextIO.closeIn is;
	    case outcome of (OK result) => result | (ERR exn) => raise exn
      end
	)
	
	(* this function sees if the interface (.sig) of the file is the actual interface of this file *)
	fun fileCheckInterfaceFile(file) =
	let val interfaceFile = getFileName(file, INTERFACE_FILE)
	in
	  if fileExists(interfaceFile) andalso 
	     readFirstLine(file) = "(*interfaceOf["^file^"]*)"
	  then true
	  else false
	end
	
	fun isInterfaceFileValid(file) = 
	let val interfaceFile = getFileName(file, INTERFACE_FILE)
	in
	  if fileExists(file) andalso 
	     fileExists(interfaceFile) andalso
	     fileNewer(interfaceFile, file) andalso 
	     fileCheckInterfaceFile(file) 
	  then true
	  else false 
	end	
	
	fun joinBaseExt(prefix, ext) = OS.Path.joinBaseExt {base = prefix, ext = ext}

    (* datatype for the result if is ok or if is error *)
    datatype 'a outcome = OK of 'a | ERR of exn
    
    (* function to write files with error handling *)
    fun withOutputOption f arg2 file =
      let val os = TextIO.openOut file
	  val outcome = (OK(f(SOME(os), arg2))) handle exn => ERR exn
      in
	    TextIO.closeOut os;
	    case outcome of (OK result) => result | (ERR exn) => raise exn
      end

    (* function to write files with error handling *)
    fun withOutput f arg2 file =
      let val os = TextIO.openOut file
	  val outcome = (OK(f(os, arg2))) handle exn => ERR exn
      in
	    TextIO.closeOut os;
	    case outcome of (OK result) => result | (ERR exn) => raise exn
      end
      
    (* function to write files with error handling *)
    fun withOutputStream f arg2 os =
      let 
	  val outcome = (OK(f(os, arg2))) handle exn => ERR exn
      in
	    case outcome of (OK result) => result | (ERR exn) => raise exn
      end
      
    (* function to write files with error handling *)
    fun withOut f arg2 =
      let val outcome = (OK(f(TextIO.stdErr, arg2))) handle exn => ERR exn
      in
	    case outcome of (OK result) => result | (ERR exn) => raise exn
      end		    
      
				    	    
  end (* structure Control *)
