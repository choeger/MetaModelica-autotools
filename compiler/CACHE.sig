(* Cache.sig *)

signature CACHE =
  sig

	structure Absyn  : ABSYN
    structure StrDict: STR_DICT
    
    datatype visibility  = PUBLIC | PROTECTED | BOTH
    datatype restriction = FUN    | REC
    datatype ftype = RML | MOD | SRZ    
    
 	type poz = (int*int*int)
   
    (* implement a cache like a key * value dictionary *)
    datatype fileInfo = INFO of 
    { 
		main         : Absyn.program, 
		imports      : (poz*poz*visibility) StrDict.dict, 
		externals    : (poz*poz) StrDict.dict, 
		restrictions : (restriction*visibility) StrDict.dict,
		reordered    : bool ref,
		elaborated   : bool ref		
	}
    
    datatype entry = ENTRY of 
    { 
        moduleId     : Absyn.ident,
		program      : fileInfo option,
		interface    : fileInfo option
		(* we can add more things here if we need them *)
	}
	
    type cache      = entry StrDict.dict ref
    type repository = cache StrDict.dict ref
     
    val rmlCache  : string
    val modCache  : string
    val srzCache  : string
    
    (* create a new cache in the main repository *)
    val  new           : repository * string -> repository
    (* creates new entry *) 
    val  makeEntry      : Absyn.ident * fileInfo option * fileInfo option -> entry 
                         
    val  makeFileInfo : Absyn.program * 
                         (poz*poz*visibility) StrDict.dict  *  
                         (poz*poz) StrDict.dict  *
                         (restriction*visibility) StrDict.dict *
                         bool ref *
                         bool ref
                         -> fileInfo 
                         
    (* insert a new entry *)
    val  insert        : repository * string * string * entry -> unit
    (* find *)
    val  find          : repository * string * string -> entry option
    (* get the entire cache *)
    val  getCache      : repository * string -> cache option
    (* get an entry in the cache *)
    val  getCacheEntry : repository * string * string -> entry option
    (* get an entry that should be there! *)
    val  getEntry      : repository * string * string -> entry

	datatype explain = RML_I_NF | RML_M_NF | MOD_I_NF | MOD_M_NF | SRZ_I_NF | SRZ_M_NF    
	exception CacheException of explain
	
    val rmlI : entry -> Absyn.module
    val rmlI_info : entry -> fileInfo
    val hasRML_I : entry -> bool
    val rmlM : entry -> Absyn.module
    val rmlM_info : entry -> fileInfo
    val hasRML_M : entry -> bool
    
    val modI : entry -> Absyn.modelica
    val modI_info : entry -> fileInfo
    val modM : entry -> Absyn.modelica
    val modM_info : entry -> fileInfo
    
    val srzI : entry -> Absyn.module
    val srzI_info : entry -> fileInfo
    val hasSRZ_I : entry -> bool    
    val srzM : entry -> Absyn.module
    val srzM_info : entry -> fileInfo
    val hasSRZ_M : entry -> bool

	val getImports: fileInfo -> (poz*poz*visibility) StrDict.dict
	val getRestrictions: fileInfo -> (restriction*visibility) StrDict.dict
	val getExternals: fileInfo -> (poz*poz) StrDict.dict
	val getModId: entry -> Absyn.ident
    
  end (* signature CACHE *)
