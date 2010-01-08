(* CacheFn.sml *)

functor CacheFn(
	structure StrDict : STR_DICT
	structure Util    : UTIL
	) : CACHE =
  struct

    structure StrDict = StrDict
	structure Absyn   = Absyn
	
	fun bug s = Util.bug("CacheFn."^s)
	
    datatype visibility  = PUBLIC | PROTECTED | BOTH
    datatype restriction = FUN | REC
    datatype ftype = RML | MOD | SRZ
    
    type poz = (int*int*int)
    
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
    
    val rmlCache  = "rmlCache"
    val modCache  = "modCache"
    val srzCache  = "srzCache"
            
    fun new(repository, mainKey) = 
    (
		ref(StrDict.insert(!repository, mainKey, ref StrDict.empty))
    )
        
    fun makeEntry(moduleId, program, interface) = 
    ENTRY
    {
		moduleId     = moduleId,
		program      = program,
		interface    = interface 
    }    
    
    (*
		main         : Absyn.program, 
		imports      : (poz*poz*visibility) StrDict.dict, 
		externals    : (poz*poz) StrDict.dict, 
		restrictions : (restriction*visibility) StrDict.dict,
		reordered    : bool ref,
		elaborated   : bool ref		
    
    *)
    fun makeFileInfo(main, imports, externals, restrictions, reordered, elaborated) = 
    INFO{
		main = main,
		imports = imports,
		externals = externals,
		restrictions = restrictions,
		reordered = reordered,
		elaborated = elaborated
    }    

	fun fileBase (file) =
	  let val {base,ext} = OS.Path.splitBaseExt file
	  in
	    base
	  end

    
    fun insert(repository, mainKey, key, value) =
    (* XXX! check here if there is a value and if is, just update it! *)
    let val c = StrDict.find(!repository, mainKey)
		val k = fileBase(key)
    in
		case c of 
		  SOME(dict) =>	( dict := StrDict.insert(!dict,k,value) )
		| NONE       => ( new(repository, mainKey); insert(repository, mainKey,key,value) )
    end
     
    fun find(repository, mainKey, key) =
    let val c = StrDict.find(!repository, mainKey)
		val k = fileBase(key)
    in
		case c of 
		  SOME(dict) =>	( StrDict.find(!dict,k) )
		| NONE       => ( NONE )
    end

    fun getCache(repository, mainKey) = StrDict.find(!repository, mainKey)
    
    fun getCacheEntry(repository, mainKey, key) = 
    let val c = StrDict.find(!repository, mainKey)
		val k = fileBase(key)
    in
		case c of 
		  SOME(dict) =>	( StrDict.find(!dict,k) )
		| NONE       => ( NONE )
    end
    
    fun getEntry(repository, mainKey, file) = 
	let val k = fileBase(file)
	in
		case getCacheEntry(repository, mainKey, k) of
			SOME(entry) => entry
		|	NONE => bug("getEntry: could not find file: "^file^" in cache: "^mainKey^"!")
	end
   
   
	datatype explain = RML_I_NF | RML_M_NF | MOD_I_NF | MOD_M_NF | SRZ_I_NF | SRZ_M_NF    
	exception CacheException of explain
	
    fun rmlI(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(RML_I_NF)
		|	(_,SOME(INFO{main=Absyn.RML_FILE(_,module),...})) => module
		|	(SOME(INFO{main=Absyn.RML_FILE(_,module),...}),_) => module
		|	(_,_) => raise CacheException(RML_I_NF)
    end
    
    fun rmlI_info(ENTRY{program,interface,...}) = 
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(RML_I_NF)
		|	(_,SOME(x)) => x
		|	(SOME(x),_) => x
    end
    
    fun rmlM(ENTRY{program,interface,...}) = 
    let 
    in
		case (program,interface) of
			(NONE, _) => raise CacheException(RML_M_NF)
		|	(SOME(INFO{main=Absyn.RML_FILE(_,module),...}),_) => module
		|	(_,_) => raise CacheException(RML_M_NF)
    end
    
    fun rmlM_info(ENTRY{program,interface,...}) = 
    let 
    in
		case (program,interface) of
			(NONE, _) => raise CacheException(RML_M_NF)
		|	(SOME(x),_) => x
    end

    fun hasRML_I(ENTRY{program,interface,...}) =
    let in
		case (program,interface) of
			(NONE, NONE) => false
		|	(_,SOME(INFO{main=Absyn.RML_FILE(_,module),...})) => true
		|	(SOME(INFO{main=Absyn.RML_FILE(_,module),...}),_) => true
		|	(_,_) => false
    end
    
    fun hasRML_M(ENTRY{program,interface,...}) = 
    let in
		case (program,interface) of
			(NONE, _) => false
		|	(SOME(INFO{main=Absyn.RML_FILE(_,module),...}),_) => true
		|	(_,_) => false
    end

		
    fun modI(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(MOD_I_NF)
		|	(SOME(INFO{main=Absyn.MOD_FILE(_,modelica),...}),_) => modelica
		|	(_,_) => raise CacheException(MOD_I_NF)
    end
    
    fun modI_info(ENTRY{program,interface,...}) = 
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(MOD_I_NF)
		|	(SOME(x),_) => x
		|	(_,_) => raise CacheException(MOD_I_NF)
    end
		
    fun modM(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(MOD_M_NF)
		|	(SOME(INFO{main=Absyn.MOD_FILE(_,modelica),...}),_) => modelica
		|	(_,_) => raise CacheException(MOD_M_NF)
    end
    
    fun modM_info(ENTRY{program,interface,...}) = 
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(MOD_M_NF)
		|	(SOME(x),_) => x
		|	(_,_) => raise CacheException(MOD_M_NF)
    end
    
    
    fun srzI(ENTRY{program,interface,...}) =
    let in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(SRZ_I_NF)
		|	(_,SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...})) => module
		|	(SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...}),_) => module
		|	(_,_) => raise CacheException(SRZ_I_NF)
    end
    
    fun srzI_info(ENTRY{program,interface,...}) = 
    let in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(SRZ_I_NF)
		|	(_,SOME(x)) => x
		|	(SOME(x),_) => x
    end
    
    fun srzM(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(SRZ_M_NF)
		|	(SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...}),_) => module
		|	(_,_) => raise CacheException(SRZ_M_NF)
    end

    fun srzM_info(ENTRY{program,interface,...}) = 
    let in
		case (program,interface) of
			(NONE, NONE) => raise CacheException(SRZ_M_NF)
		|	(SOME(x),_) => x
		|	(_,_) => raise CacheException(SRZ_M_NF)
    end
    
    fun hasSRZ_I(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => false
		|	(_,SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...})) => true
		|	(SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...}),_) => true
		|	(_,_) => false
    end
    
    fun hasSRZ_M(ENTRY{program,interface,...}) =
    let 
    in
		case (program,interface) of
			(NONE, NONE) => false
		|	(SOME(INFO{main=Absyn.SRZ_FILE(_,Absyn.SERIALIZED(_,SOME(module))),...}),_) => true
		|	(_,_) => false
    end
   
	fun getImports(INFO{imports,...}) = imports
	fun getRestrictions(INFO{restrictions,...}) = restrictions
	fun getExternals(INFO{externals,...}) = externals
	fun getModId(ENTRY{moduleId,...}) = moduleId
		   
  end (* functor CacheFn *)