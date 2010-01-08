(* static/statelab.sml *)
(* adrpo 2004-11-29 changed this file quite a lot for better type error locations *)

functor StatElabFn(structure Util : UTIL
		   structure StatObj : STAT_OBJ
		   structure Control : CONTROL
		   structure Instrument : ABSYN_INSTRUMENTED
		   structure AbsynPrint : ABSYN_PRINT
		   structure AbsynPersist : ABSYN_PERSIST
		   structure Cache : CACHE
		   sharing type StatObj.Absyn.Source.source = Cache.Absyn.Source.source
		   sharing type Instrument.Absyn.pat = Cache.Absyn.pat
		   sharing type Instrument.Absyn.info = Cache.Absyn.info
		   sharing type StatObj.Absyn.IdentDict.Key.ord_key = Cache.Absyn.ident
		   
) : STAT_ELAB =

  struct
  
    type repository = Cache.repository

	structure Absyn = Cache.Absyn
    structure IdentDict = StatObj.Absyn.IdentDict 
    structure Ty = StatObj.Ty
    structure TyFcn = StatObj.TyFcn
    structure TyScheme = StatObj.TyScheme
    
    fun bug s = Util.bug("StatElab."^s)
   
    fun uerror s = Util.error("StatElab."^s)    
    
	val debugFlag = false	
	fun debug s = if (debugFlag) then Util.outStdErr ("StatElabFn."^s) else ()    

    (* Maintaining a dynamically-bound "current source" object *)
    val currentSource = ref Absyn.Source.dummy

    fun withSource(newSource, f) =
      let val oldSource = !currentSource
	  val _ = currentSource := newSource
      in
		let val result = f()
			val _ = currentSource := oldSource
		in
		 result
		end handle exn => (currentSource := oldSource; raise exn)
      end

	fun getLoc (sp, ep) = 
	let val {fileName, sline, scolumn, eline, ecolumn} = Absyn.Source.getLoc(!currentSource, sp, ep)
	in
		(fileName, sline, scolumn, eline, ecolumn)
	end
      
    (* adrpo 2004-12-09 *)
    fun printOs(SOME(os), s) = TextIO.output(os, s)
      | printOs(NONE, s) = ()


    (* Print plain error messages *)

    fun sayErr s = TextIO.output(TextIO.stdErr, s)

    (* Print error messages with source-file contexts *)

    fun sayError'(source, msg, left, right) =
      Absyn.Source.sayMsg source ("Error: "^msg, left, right)

    fun sayIdError'(source, msg, name, Absyn.INFO(left, right)) =
      sayError'(source, msg^name, left, right)

    fun sayError(msg, left, right) = sayError'(!currentSource, msg, left, right)

    fun sayIdError(msg, name, Absyn.INFO(left, right)) =
      sayError(msg^name, left, right)

    (* Generate error messages, then raise StaticElaborationError *)

    exception StaticElaborationError

    fun error'(source, msg, left, right) =
      (sayError'(source, msg, left, right);
       raise StaticElaborationError)

    fun idError'(source, msg, name, ctxInfo) =
      (sayIdError'(source, msg, name, ctxInfo);
       raise StaticElaborationError)

    fun error(msg, left, right) =
      (sayError(msg, left, right);
       raise StaticElaborationError)

    fun idError(msg, name, ctxInfo) =
      (sayIdError(msg, name, ctxInfo);
       raise StaticElaborationError)

    fun idUndeclaredError(kind, Absyn.IDENT(name, ctxInfo)) = 
		idError("no declaration for " ^ kind ^ " ", name, ctxInfo)
		
    fun idUnboundError(kind, Absyn.IDENT(name, ctxInfo)) = 
		idError("unbound " ^ kind ^ " ", name, ctxInfo)

    fun idRebindError(
            kind, 
            here as Absyn.IDENT(namehere, ctxInfoHere), 
            there as Absyn.IDENT(namethere, ctxInfoThere)) =
      (sayIdError("rebinding of " ^ kind ^ " ", namehere, ctxInfoHere);
       idError("this is the other binding of ", namethere, ctxInfoThere))

    fun lidError(msg, Absyn.LONGID(SOME(Absyn.IDENT(name1,_)), Absyn.IDENT(name2,_), ctxInfo)) 
		= idError(msg, name1^"."^name2, ctxInfo)
    | lidError(msg, Absyn.LONGID(NONE, Absyn.IDENT(name,_), ctxInfo)) 
		= idError(msg, name, ctxInfo)

    (* Add context to type errors *)
    fun sayTyErrExplain(Ty.TY_ERROR(ty, why)) =
	  (sayErr "type "; Ty.printType ty; sayErr " "; sayErr why)
      | sayTyErrExplain(Ty.TY_INST(tyvar, ty, why)) =
	  (sayErr "type variable "; Ty.printType(Ty.VAR tyvar);
	   sayErr " cannot be bound to type "; Ty.printType' ty;
	   sayErr "\nreason: "; sayErr why)
      | sayTyErrExplain(Ty.TY_DIFFER(ty1, ty2, why)) =
	  (sayErr "type "; Ty.printType ty1;
	   sayErr " differs from:\ntype "; Ty.printType' ty2;
	   sayErr "\nreason: different "; sayErr why)

    (* adrpo added 2004-11-29 ctxInfo, for better type error locations *)
    fun sayTyErr(explain, ctxKind, name, ctxInfo) =
      (sayErr "\n";
       sayIdError("while processing " ^ ctxKind ^ " ", name, ctxInfo);
       sayTyErrExplain explain;
       sayErr "\n")

    fun strayExnBug(exn, name, ctxInfo) =
      (sayErr "\n";
       sayIdError("exception " ^ General.exnMessage exn ^ " while here: ", name, ctxInfo);
       bug "strayExn")

    (* some stuff.. *)

    fun tyvarRigid tyvar = Ty.RIGID(Absyn.identName tyvar)

    fun sameTyvar tyvar tyvar' = Absyn.identEqual(tyvar, tyvar')

    (* Verify that the identifier isn't bound *)
    fun assert_unbound(env, id, kind) =
      case IdentDict.find'(env, id)
		of NONE => ()
		| SOME(id',_) => idRebindError(kind, id, id')

    fun assert_unbound_tycon(TE,id) = assert_unbound(TE,id,"type constructor")
    fun assert_unbound_con(VE,id) = assert_unbound(VE,id,"data constructor")
    fun checkVarNotBound(VE,id) = assert_unbound(VE,id,"variable")
    fun assert_unbound_modid(ME, id) = assert_unbound(ME,id,"module id")

    (* Assert that an id is bound as a relation; return type scheme *)
    fun assert_rel(id, StatObj.VALSTR{vk,sigma,...}) =
      case vk
		of StatObj.REL => sigma
		| _ => idError("not a relation: ", 
						Absyn.identName id, 
						Absyn.identCtxInfo id)

    (* Various short/longid lookup functions *)

    fun lookupDeclared(env, id, kind) =
      case IdentDict.find(env, id)
		of SOME attr => (* print ("\nfound :"^(Absyn.identName id)^" as "^kind); *) attr
		| NONE => idUndeclaredError(kind, id)

    fun lookupDeclaredVar(VE, var)     = (* print "\nsearch VE:"; *) lookupDeclared(VE, var, "variable")

    fun lookup(env, id, kind) =
      case IdentDict.find(env, id)
		of SOME attr => (* print ("\nfound :"^(Absyn.identName id)^" as "^kind); *) attr
		| NONE => idUnboundError(kind, id)

    fun lookupVar(VE, var)     = (* print "\nsearch VE:"; *) lookup(VE, var, "variable")
    fun lookupTycon(TE, tycon) = (* print "\nsearch TE:"; *) lookup(TE, tycon, "type constructor")
    fun lookupModid(ME, modid) = (* print "\nsearch ME:"; *) lookup(ME, modid, "module id")

    fun lookup_modid_VE(ME, modid) =
      let val StatObj.MODSTR{VE,...} = lookupModid(ME, modid)
      in
		VE
      end

    fun lookup_longid(ME, VE, Absyn.LONGID(modid_opt, id, _)) =
      case modid_opt
		of NONE => (* instead of searching VE_init+VE, search them separately *)
			(case IdentDict.find(VE, id)
			of SOME attr => attr
			| NONE => lookupVar(StatObj.VE_init, id))
		| SOME modid => lookupVar(lookup_modid_VE(ME, modid), id)

    (* Looking up (long) type constructors *)
    fun lookup_longtycon(ME, TE, Absyn.LONGID(modid_opt, tycon, _)) =
      let fun modstrTE(StatObj.MODSTR{TE,...}) = TE
	  fun tystrTheta(StatObj.TYSTR{theta,...}) = theta
      in
		case modid_opt
		of NONE => (* instead of searching TE_init+TE, search them separately *)
			(case IdentDict.find(TE, tycon)
			of SOME(StatObj.TYSTR{theta,...}) => theta
			| NONE => tystrTheta(lookupTycon(StatObj.TE_init, tycon)))
		| SOME modid =>
			tystrTheta(lookupTycon(modstrTE(lookupModid(ME, modid)), tycon))
      end

    (**************************** Elaborate an entire module ***************************)
    fun elab_module(os, main_module, repository) =
    
	let val _ = debug("elab_module\n")
			
    (* Annotate "with" specifications and declarations with actual interfaces *)
    fun annotate_with(file, ri) =
      let val module = 
			let val entryRML = Cache.getCacheEntry(repository, Cache.rmlCache, file)
				val entrySRZ = Cache.getCacheEntry(repository, Cache.srzCache, file)
			in
				case entryRML of 
					SOME(e) => Cache.rmlI(e)
				|	NONE => (case entrySRZ of
								SOME(e) => Cache.srzI(e)
							|	NONE =>	
								bug("annotate_with: could not find file: "^file^" in cache"))
			end
      val Absyn.MODULE(i as Absyn.INTERFACE({specs,source,...}, _), _, _) = module
      in
		ri := i;
		List.app annotate_with_spec specs
      end

    and annotate_with_spec (Absyn.WITHspec(file, ri, _)) = annotate_with(file, ri)
      | annotate_with_spec (_) = ()

    fun annotate_with_dec (Absyn.WITHdec(file, ri, _)) = annotate_with(file, ri)
      | annotate_with_dec (_) = ()

    fun annotate_module(Absyn.MODULE(Absyn.INTERFACE({specs,...}, _), decs, _)) =
      (
      List.app annotate_with_spec specs;
      List.app annotate_with_dec decs
      )
      
	(* returns the named arguments from a type sequence *)
	fun getNamedArgumentsInTys([]) = []
	|	getNamedArgumentsInTys((x as Absyn.NAMEDty(_))::rest) = x::getNamedArgumentsInTys(rest)
	|	getNamedArgumentsInTys(_::rest) = getNamedArgumentsInTys(rest)

    (* 
    adrpo: this function checks if all the named arguments in the type sequence are unique
    *)
	fun checkUniqueNamedArgsInTys(tyseq) = 
	let val namedArgs = getNamedArgumentsInTys(tyseq)
		fun checkUnique([], []) = ()
		|	checkUnique(left, []) = ()
		|	checkUnique(left, (current as Absyn.NAMEDty(id1, ty1, info1))::right) = 
		let fun is_there(Absyn.NAMEDty(id2, ty2, ident2)) = Absyn.identEqual(id1, id2)
			|	is_there(_) = false
		in
			case List.find is_there (left @ right) of
				SOME(Absyn.NAMEDty(ident, pat, infoIdent)) => 
					idRebindError("named argument in type: ", id1, ident)
			|	_ => checkUnique(left @ [current], right) 
		end
		|	checkUnique(left, current::right) = (checkUnique(left @ [current], right))		
		
	in
	  checkUnique([], tyseq)
	end	      
      
    (* Elaborate a type expression *)
    fun elab_ty ME TE tyvarset_opt =
      let fun elab(Absyn.VARty(tyvar, _)) =
		(case tyvarset_opt
		   of SOME tyvarset	=>
			if List.exists (sameTyvar tyvar) tyvarset
			  then Ty.VAR(tyvarRigid tyvar)
			  else idUnboundError("type variable", tyvar)
		    | NONE => Ty.VAR(tyvarRigid tyvar))
	    | elab(Absyn.CONSty(tyseq, longtycon, _)) =
		let val tyfcn = lookup_longtycon(ME, TE, longtycon)
			val _ = checkUniqueNamedArgsInTys(tyseq) (* adrpo added *)
		in
		  if length tyseq = TyFcn.arity tyfcn then
		    TyFcn.apply(tyfcn, map elab tyseq)
		  else
		    lidError("wrong number of arguments to type function ",
			     longtycon)
		end
	    | elab(Absyn.TUPLEty(tyseq, _)) =
	      (
	        (* check also for name duplicates and named argument mixing with positional *) 
	        checkUniqueNamedArgsInTys(tyseq); (* adrpo added *)
			Ty.TUPLE(map elab tyseq)
		  )
	    | elab(Absyn.RELty(domtys,codtys, _)) =
		  (
	    	(* check also for name duplicates and named argument mixing with positional *) 
	        checkUniqueNamedArgsInTys(domtys); (* adrpo added *)
	        checkUniqueNamedArgsInTys(codtys); (* adrpo added *)	        
			Ty.REL(map elab domtys, map elab codtys)
		  )
		| elab(Absyn.NAMEDty(Absyn.IDENT(id_str, _), ty, _)) = Ty.NAMED(id_str, elab ty)
      in
		elab
      end

    (* Check that a tyvarseq contains no duplicates *)

    fun nodups([]) = ()
      | nodups(tyvar::tyvarseq) =
	  if List.exists (sameTyvar tyvar) tyvarseq
	    then idError("multiply bound type variable ", 
					Absyn.identName tyvar, 
					Absyn.identCtxInfo tyvar)
	    else nodups tyvarseq

    (* Elaborate a sequence of type bindings *)

    fun elab_typbinds(ME, TE, TE1, typbinds) =
      let fun elab(Absyn.TYPBIND(tyvarseq, tycon, ty, _), TE1) =
	    let val _ = assert_unbound_tycon(TE, tycon)
		val _ = assert_unbound_tycon(TE1, tycon)
		val _ = nodups tyvarseq
		val tau = elab_ty ME (IdentDict.plus(TE,TE1)) (SOME tyvarseq) ty
		val theta = TyFcn.lambda(map tyvarRigid tyvarseq, tau)
		val tystr = StatObj.TYSTR{theta=theta, abstract=false}
	    in
	      IdentDict.insert(TE1, tycon, tystr)
	    end
      in
		List.foldl elab TE1 typbinds
      end

    (* Elaborate a withbind *)

    fun elab_withbind(ME, TE, []) = IdentDict.empty
      | elab_withbind(ME, TE, typbinds) =
	  elab_typbinds(ME, TE, IdentDict.empty, typbinds)

    (* Elaborate a datbind to a skeletal TE.
     * Also build an annotated version of the datbinds, datbindsA,
     * which is a list of tuples (datbind,t,tau,theta).
     *)
    fun elab_datbind_skel(TE, TE_skel, modid, datbinds) =
      let fun assert_unbound_or_abstract(TE, tycon) =
	    case IdentDict.find'(TE, tycon)
	      of NONE => ()
	       | SOME(_, StatObj.TYSTR{abstract=true,...}) => ()
	       | SOME(tycon', StatObj.TYSTR{abstract=false,...}) =>
		  idRebindError("type constructor", tycon, tycon')
	  fun elab([], TE_skel, datbindsA) = (TE_skel, datbindsA)
	    | elab(datb::datbinds, TE_skel, datbindsA) =
		let val Absyn.DATBIND(tyvarseq, tycon, _, _) = datb
		    val _ = assert_unbound_or_abstract(TE, tycon)
		    val _ = assert_unbound_tycon(TE_skel, tycon)
		    val _ = nodups tyvarseq
		    val alphaseq = map tyvarRigid tyvarseq
		    val t = Ty.TYNAME{modid=Absyn.identName modid,
				      tycon=Absyn.identName tycon,
				      eq=ref Ty.MAYBE}
		    val tau = Ty.CONS(map Ty.VAR alphaseq, t)
		    val theta = TyFcn.lambda(alphaseq, tau)
		    val tystr = StatObj.TYSTR{theta=theta, abstract=false}
		in
		  elab(datbinds,
		       IdentDict.insert(TE_skel, tycon, tystr),
		       (datb,t,tau,theta) :: datbindsA)
		end
      in
		elab(datbinds, TE_skel, [])
      end

    (* Elaborate a conbind to a CE.
     * Also build a PreCE, which contains the argument types of every
     * non-constant constructor. These are the types that must be checked
     * later when determining if this datatype's tyname admits equality.
     *)
    fun elab_conbind(ME, TE, VE, CE, tyvarseq_opt, res_tau, conbinds) =
      let fun conbind_con(Absyn.CONcb(con, _)) = con
	    | conbind_con(Absyn.CTORcb(con, _, _)) = con
	  fun conbind_tau(Absyn.CONcb _, PreCE) = (res_tau, PreCE)
	    | conbind_tau(Absyn.CTORcb(_, tyseq, _), PreCE) =
		let val domtaus = map (elab_ty ME TE tyvarseq_opt) tyseq
			val _ = checkUniqueNamedArgsInTys(tyseq)
		in
		  (Ty.REL(domtaus, [res_tau]), domtaus::PreCE)
		end
	  fun elab(CE, PreCE, []) = (CE, PreCE)
	    | elab(CE, PreCE, cb::conbind) =
		let val con = conbind_con cb
		    val (tau,PreCE) = conbind_tau(cb, PreCE)
		    val _ = assert_unbound_con(VE, con)
		    val _ = assert_unbound_con(CE, con)
		    val sigma = TyScheme.genAll tau
		    val bnd = StatObj.VALSTR{vk=StatObj.CON,sigma=sigma,localVE=IdentDict.empty, global=false}
		    val CE' = IdentDict.insert(CE, con, bnd)
		in
		  elab(CE', PreCE, conbind)
		end
      in
		elab(CE, [], conbinds)
      end

    (* Elaborate a datbind to the final TE and VE.
     * This actually uses an annotated datbind [(datbind,t,tau,theta) list]
     * in order to save some work.
     * Also build a new annotated datbind, datbindsB, which is a list
     * of (t,PreCE)-pairs. These are later input to maximises_equality.
     *)
    fun elab_datbind_CE(ME, TE, VE, datbindsA) =
      let fun elab(VE, [], TE_data, datbindsB) = (TE_data, VE, datbindsB)
	    | elab(VE, datbA::datbindsA, TE_data, datbindsB) =
		let val (datb,t,tau,theta) = datbA
		    val Absyn.DATBIND(tyvarseq, tycon, conbind, _) = datb
		    val (CE,PreCE) = elab_conbind(ME, TE, VE, IdentDict.empty,
						  SOME tyvarseq, tau, conbind)
		    val VE' = IdentDict.plus(VE, CE)
		    val tystr = StatObj.TYSTR{theta=theta, abstract=false}
		in
		  elab(VE',
		       datbindsA,
		       IdentDict.insert(TE_data, tycon, tystr),
		       (t,PreCE) :: datbindsB)
		end
      in
		elab(VE, datbindsA, IdentDict.empty, [])
      end

    (* Verify that a new TE from a datbind maximises equality.
     * Initially assume that all new type names may admit equality.
     * (Done above in elab_datbind_skel.)
     * For every (t,CE) bound in TE where t admits equality:
     *   If there exists a constructor con in CE, such that CE(con)
     *   does not admit equality, then set t to not admit equality.
     * Repeat until no further changes occur.
     *
     * For simplicity and performance, this actually operates on a list
     * of (t,PreCE)-pairs instead of the real TE.
     *)
    fun maximises_equality datbindsB =
      let fun tauAdmitsEq tau = Ty.admitsEq(tau, true)
	  fun domtausAdmitEq domtaus = List.all tauAdmitsEq domtaus
	  fun constructorsAdmitEq PreCE = List.all domtausAdmitEq PreCE
	  fun foundEq((Ty.TYNAME{eq,...},PreCE), othersFoundEq) =
	    case !eq
	      of Ty.MAYBE =>
		  if constructorsAdmitEq PreCE then othersFoundEq
		  else (eq := Ty.NEVER; false)
	       | _ => othersFoundEq
	  fun findFixPoint() =
	    if List.foldl foundEq true datbindsB then ()
	    else findFixPoint()
      in
		findFixPoint()
      end

    (* Elaborate a datatype declaration/specification *)

    fun elab_datatype(ME, TE, VE, modid, datbinds, withbind) =
      let val (TE_skel,datbindsA) = elab_datbind_skel(TE,IdentDict.empty,modid,datbinds)
	  val TE_with = elab_withbind(ME, IdentDict.plus(TE,TE_skel), withbind)
	  val (TE_data,VE',datbindsB) = elab_datbind_CE(ME, IdentDict.plus(IdentDict.plus(TE,TE_skel),TE_with), VE, datbindsA)
	  val _ = maximises_equality datbindsB
	  val TE' = IdentDict.plus(IdentDict.plus(TE,TE_data),TE_with)
      in
		(TE', VE')
      end

    (* Check that an identifier may be bound as a variable *)

    fun checkNotCon(sourceVE, VE, var) =
      case IdentDict.find'(VE, var)
		of SOME(con, StatObj.VALSTR{vk=StatObj.CON,...}) =>
	    (sayIdError("cannot bind constructor as variable: ", 
					Absyn.identName var, 
					Absyn.identCtxInfo var);
	     idError'(sourceVE, "this is the binding of ", 
	              Absyn.identName con, 
	              Absyn.identCtxInfo con))
		| _ => ()

    fun checkVar(VE_outer, VE_inner, var) =
      (checkVarNotBound(VE_inner, var);
       checkNotCon(!currentSource, VE_outer, var);
       checkNotCon(StatObj.sourceInit, StatObj.VE_init, var))

    (* Elaborate a sequence of specifications *)

    fun elab_specs(ME, TE, VE, modid, specs) =
      let val _ = debug("elab_specs\n") 
        fun elab(ME, TE, VE, []) = (ME,TE,VE)
	    | elab(ME, TE, VE, Absyn.WITHspec(_, interface, _)::specs) =
		let val (_,TE',VE',modid') = elab_interface(ME, !interface)
		    val Absyn.INTERFACE({source=source',...}, _) = !interface
		    val modstr = StatObj.MODSTR{TE=TE', VE=VE', source=source'}
		    val ME' = IdentDict.insert(ME, modid', modstr)
		in
		  elab(ME', TE, VE, specs)
		end
	    | elab(ME, TE, VE, Absyn.ABSTYPEspec(eq, tyvarseq, tycon, _)::specs) =
		let val _ = assert_unbound_tycon(TE, tycon)
		    val _ = nodups tyvarseq
		    val alphaseq = map tyvarRigid tyvarseq
		    val t = Ty.TYNAME{modid=Absyn.identName modid,
				      tycon=Absyn.identName tycon,
				      eq=ref(if eq then Ty.MAYBE else Ty.NEVER)}
		    val tau = Ty.CONS(map Ty.VAR alphaseq, t)
		    val theta = TyFcn.lambda(alphaseq, tau)
		    val tystr = StatObj.TYSTR{theta=theta, abstract=true}
		    val TE' = IdentDict.insert(TE, tycon, tystr)
		in
		  elab(ME, TE', VE, specs)
		end
	    | elab(ME, TE, VE, Absyn.TYPEspec(typbinds, _)::specs) =
		let val TE' = elab_typbinds(ME, IdentDict.empty, TE, typbinds)
		in
		  elab(ME, TE', VE, specs)
		end
	    | elab(ME, TE, VE, Absyn.DATAspec(datbinds, withbind, _)::specs) =
		let val (TE',VE') = elab_datatype(ME,TE,VE,modid,datbinds,withbind)
		in
		  elab(ME, TE', VE', specs)
		end
	    | elab(ME, TE, VE, Absyn.VALspec(var, ty, _)::specs) =
		let val _ = checkVar(IdentDict.empty, VE, var)
		    val tau = elab_ty ME TE NONE ty
		    val sigma = TyScheme.genAll tau
		    val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=true}
		    val VE' = IdentDict.insert(VE, var, bnd)
		in
		  elab(ME, TE, VE', specs)
		end
	    | elab(ME, TE, VE, Absyn.RELspec(var, ty, _)::specs) =
		let val _ = checkVar(IdentDict.empty, VE, var)
		    val tau = elab_ty ME TE NONE ty
		    val _ =
		      case tau
				of Ty.REL(_,_) => ()
				| _ => idError("relation specified to have non-relational type: ", 
								Absyn.identName var,
								Absyn.identCtxInfo var)
		    val sigma = TyScheme.genAll tau
		    val bnd = StatObj.VALSTR{vk=StatObj.REL, sigma=sigma, localVE=IdentDict.empty, global=true}
		    val VE' = IdentDict.insert(VE, var, bnd)
		in
		  elab(ME, TE, VE', specs)
		end
      in
		elab(ME, TE, VE, specs)
      end

    (* Elaborate an interface *)
    and elab_interface(ME, Absyn.INTERFACE({modid,specs,source}, _)) =
      let 
      val _ = debug("elab_interface of "^(Absyn.identName modid)^"\n")
      val _ = assert_unbound_modid(ME, modid)
	  fun elab() =
	    let val (ME',TE,VE) = elab_specs(StatObj.ME_init, 
	                                     IdentDict.empty, 
	                                     IdentDict.empty,
										 modid, 
										 specs)
	    in
	      (ME',TE,VE,modid)
	    end
      in
		withSource(source, elab)
      end

    (* Compute the type of a literal *)
    fun elab_lit(Absyn.CCONlit _) = StatObj.tau_char
      | elab_lit(Absyn.ICONlit _) = StatObj.tau_int
      | elab_lit(Absyn.RCONlit _) = StatObj.tau_real
      | elab_lit(Absyn.SCONlit _) = StatObj.tau_string

    (* Assert that an id is bound as a constructor; return instantiated sigma *)
    fun fresh_longcon(ME, VE, longcon as Absyn.LONGID(_, con, _)) =
      let val StatObj.VALSTR{vk,sigma,...} = lookup_longid(ME, VE, longcon)
      in
		case vk
		 of StatObj.CON => TyScheme.instFree sigma
		 | _ => idError("not a data constructor: ", 
						Absyn.identName con, 
						Absyn.identCtxInfo con)
      end

    (* Assert that this is constant constructor *)

    fun assert_constant(tau, longcon as Absyn.LONGID(_, con, _)) =
      case tau
		of Ty.CONS(_,_) => ()
		| _ => idError("data constructor isn't constant: ",
						Absyn.identName con, 
						Absyn.identCtxInfo con)
	
	(* adrpo check if this contains named arguments in patterns *)
	fun containsNamed([]) = false
	|	containsNamed(pat::restpatseq) = 
		(
		case pat of 
			Absyn.NAMEDpat(_) => 
			( (* print ("containsNamed=true -> "^(Instrument.getPatAsString(pat))^"\n"); *)
			  true 
			)
		(* if list or cons is not a named pat *)
		|	Absyn.STRUCTpat(x, patseq, _, _) =>
		    (
				case x of 
					SOME(Absyn.LONGID(NONE, Absyn.IDENT("cons", _), _))
					=> 	containsNamed(restpatseq)
				|	SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)), Absyn.IDENT("cons", _), _))
					=> 	containsNamed(restpatseq)
				|	SOME(Absyn.LONGID(NONE, Absyn.IDENT("list", _), _))
					=> 	containsNamed(restpatseq)
				|	SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)), Absyn.IDENT("list", _), _))
					=> 	containsNamed(restpatseq)
				|	NONE (* tuple constructor *)
					=> 	containsNamed(restpatseq)
				|	_ (* normal constructor *)
					=> 	containsNamed(restpatseq)
			)
		|	_ => containsNamed(restpatseq)
	    )	    
	    
	fun getNamedArgumentInPattern(id, []) = NONE (* error, should be there *)
	|	getNamedArgumentInPattern(id, (x as Absyn.NAMEDpat(id_check, _, _))::rest) =
		if id = Absyn.identName(id_check) 
		then SOME(x)
		else getNamedArgumentInPattern(id, rest)
	|	getNamedArgumentInPattern(id, _::rest) = getNamedArgumentInPattern(id, rest)
	
	(* 
	adrpo this function generates a positional pattern conform to the datatype 
	constructor with the named arguments from patseq bound to the right places 
	*)
    fun generatePat(_, []) = [](* if the actual pattern is empty, don't bother *)
    |	generatePat(tyargs_taus, patseq) =
	let fun generatePatternFromTau(Ty.VAR(tyvar)) = Absyn.WILDpat(Absyn.dummyInfo)
		|	generatePatternFromTau(Ty.TUPLE(tys)) = 
			let val pats = map generatePatternFromTau tys
			in
			Absyn.STRUCTpat(
				NONE,
				pats,
				ref (pats),
				Absyn.dummyInfo)
			end
		|	generatePatternFromTau(Ty.REL(domtys, codtys)) = 
			let val pats = map generatePatternFromTau domtys
			in
			Absyn.STRUCTpat(
				NONE,
				pats,
				ref (pats),
				Absyn.dummyInfo)
			end
		|	generatePatternFromTau(Ty.CONS(tys, t as Ty.TYNAME{modid=modid,tycon=tycon,...})) = 
			let val pats = map generatePatternFromTau tys
			in
			if (List.length pats > 0)
			then
			Absyn.STRUCTpat(
				SOME(Absyn.LONGID(SOME(Absyn.rmlIdent(modid)), Absyn.rmlIdent(tycon), Absyn.dummyInfo)),
				pats,
				ref (pats),
				Absyn.dummyInfo)
			else Absyn.WILDpat(Absyn.dummyInfo)
			end
		|	generatePatternFromTau(Ty.NAMED(id, ty)) = 
			(* get the actual pattern from the patseq *)
			let val named = getNamedArgumentInPattern(id, patseq)
			in 
				case named of 
					NONE => Absyn.WILDpat(Absyn.dummyInfo) (* we didn't find it, make it WILDpat *) 
				|	SOME(x) => x
			end
		val patWithNamedArguments = map generatePatternFromTau tyargs_taus
		fun loopPositionals([], []) = []
		|	loopPositionals(hd1::rest1, hd2::rest2) = 
			let 
			in
				case hd1 of 
					Absyn.NAMEDpat(id,_,_) => 
					(
					case hd2 of
						Absyn.NAMEDpat(_) => hd1::loopPositionals(rest1, rest2) 
					|	_ => bug("named argument: "^(Absyn.identName id)^" in pattern overrides positional argument! ")
					)
				|	Absyn.WILDpat(_) => 
					(
					case hd2 of
						Absyn.NAMEDpat(_) => hd1::loopPositionals(rest1, rest2) 
					|	_ => hd2::loopPositionals(rest1, rest2)
					)
				|   _ => bug("generatePat is really weird to have this pattern here: "^(Instrument.getPatAsString(hd1)))
			end
		|	loopPositionals([], _) = bug("pattern and type have different number of components!")
		|	loopPositionals(hd1::rest1, []) = hd1::loopPositionals(rest1, [])			
    in
	  case List.hd patseq of
	    (* if the first pat is named, there are no positionals *)
		Absyn.NAMEDpat(_) => patWithNamedArguments 
	    (* we have positionals *)
	  | _ => loopPositionals(patWithNamedArguments, patseq)
    end	

	(* returns the named arguments from a pattern sequence *)
	fun getNamedArgumentsInPats([]) = []
	|	getNamedArgumentsInPats((x as Absyn.NAMEDpat(_))::rest) = x::getNamedArgumentsInPats(rest)
	|	getNamedArgumentsInPats(_::rest) = getNamedArgumentsInPats(rest)


	fun getNamedArgumentsInTyTauLst([]) = []
	|	getNamedArgumentsInTyTauLst(x::rest) =
		case x of
			Ty.VAR(tyvar) => getNamedArgumentsInTyTauLst(rest)
		|	Ty.TUPLE(tys) => 
				getNamedArgumentsInTyTauLst(tys) @ getNamedArgumentsInTyTauLst(rest)
		|	Ty.REL(domtys, codtys) => 
				getNamedArgumentsInTyTauLst(domtys) @ getNamedArgumentsInTyTauLst(rest)
		|	Ty.CONS(tys, t as Ty.TYNAME{modid=modid,tycon=tycon,...}) => 
				getNamedArgumentsInTyTauLst(tys) @ getNamedArgumentsInTyTauLst(rest)
		|	Ty.NAMED(id, ty) => id :: getNamedArgumentsInTyTauLst(rest)
		
    (* 
    adrpo: this function checks if all the named arguments in the pattern sequence are unique
    *)
	fun checkUniqueNamedArgsInPats(patseq) = 
	let val namedArgs = getNamedArgumentsInPats(patseq)
		fun checkUnique([], []) = ()
		|	checkUnique(left, []) = ()
		|	checkUnique(left, (current as Absyn.NAMEDpat(id1, pat1, info1))::right) = 
		let fun is_there(Absyn.NAMEDpat(id2, pat2, ident2)) = Absyn.identEqual(id1, id2)
			|	is_there(_) = false
		in
			case List.find is_there (left @ right) of
				SOME(Absyn.NAMEDpat(ident, pat, infoIdent)) => 
					idRebindError("named argument in pattern: ", id1, ident)
			|	_ => checkUnique(left @ [current], right) 
		end	
		|	checkUnique(left, current::right) = checkUnique(left @ [current], right)
	in
	  checkUnique([], patseq)
	end	
		
		
	fun checkExistingInTyNamedArgFromPats(patseq, args_taus) = 
	let val namedInTy  = getNamedArgumentsInTyTauLst(args_taus)
		val namedInPat = getNamedArgumentsInPats(patseq)
		fun formatNames([]) = ""
		|	formatNames(x::nil) = x
		|	formatNames(x::rest) = (x^", ")^formatNames(rest) 
		fun check_exist_in_ty([]) = ()
		|	check_exist_in_ty(Absyn.NAMEDpat(ident, pat, infoIdent)::rest) =
			let fun is_there(x) = Absyn.identName(ident) = x
			in
				if List.exists is_there namedInTy
				then (* move on *) check_exist_in_ty(rest)
				else (* report error *)
					let val pat_str = 
								Instrument.getPatAsString(
									Absyn.STRUCTpat(
										NONE, 
										patseq,
										ref [],
										Absyn.dummyInfo))
					in
					idError(
					"the named argument: "^(Absyn.identName ident)^" from pattern: "^pat_str^" does not exist in the type. ", 
					"The available names for the type components are: ["^formatNames(namedInTy)^"]",
					infoIdent)
					end
			end
		|	check_exist_in_ty(_::rest) = check_exist_in_ty(rest)
	in
		check_exist_in_ty(namedInPat)
	end

	(*
	adrpo: this function check is the order of the named arguments in pattern is correct:
	( positional*, named* ) meaning no positional after one named!
	*)
	fun checkNamedArgOrderInPat([]) = ()		
	|   checkNamedArgOrderInPat(patseq) =
    let	val startsWith = 
		    case List.hd patseq of 
				Absyn.NAMEDpat(_) => 1 (* starts with named argument *)
			|	_ => 0 (* starts with positional argument *)	
		fun loop([], _, p) = ()
		|	loop(pat::rest, continueWith, p) =
			(
			case pat of
				Absyn.NAMEDpat(_) => loop(rest, 1, pat) (* ok either way *)
			|	_ => if continueWith=0 
				        then loop(rest, 0, pat) (* that's ok, were positional arguments until now *) 
				        else  (* uups, there is a wrong mix of positional and named arguments *)
						let val pat_str = 
								Instrument.getPatAsString(
									Absyn.STRUCTpat(
										NONE, 
										patseq,
										ref [],
										Absyn.dummyInfo))
						in
						case p of 
							Absyn.NAMEDpat(id, pat_, info) =>
								idError(
									"in pattern: "^pat_str^
									", after the named argument: \""^(Absyn.identName id)^
									"\" cannot follow a positional argument. ",
									"A named argument must follow. Positional arguments can be only at the begining.",
									info)
						|	_ => idError(
									"after a named argument cannot follow a positional argument in pattern: "^pat_str^". ",
									"A named argument must follow. Positional arguments can be only at the begining.",
									Instrument.getInfoFromPat(pat))
						end
			)
	in 
		loop(patseq, startsWith, List.hd patseq)
	end
	
	fun checkNamedArgPositionInPat(patseq, args_taus) = ()
	
	(*
      checks if the pattern sequence with named arguments is conform:
      - form: ( positional*, named* )
      - also named argument must lead to a position that is not already 
        occupied by a positional argument
      - also named arguments must be unique
	*)
	fun checkNamedArgInPattern([], args_taus) = ()
	|	checkNamedArgInPattern(patseq, args_taus) =
		let val _ = checkNamedArgOrderInPat(patseq) (* check order of positional/named arguments *)
			val _ = checkUniqueNamedArgsInPats(patseq)    (* check unique of named arguments *)
			val _ = checkExistingInTyNamedArgFromPats(patseq, args_taus)
		in
			()
		end
		
	fun checkPattern([], args_taus) = ()
	|	checkPattern(patseq, args_taus) = checkNamedArgInPattern(patseq, args_taus)
	
	(* 
	if the pattern has named arguments then rewrite the 
	pattern to positional one and the result place it in 
	the patseq_ref
	*)
	fun fixPatSeq(patseq, patseq_ref, tyargs_taus) =
	(
		if containsNamed(patseq) 
		then 
		let val _ = checkPattern(patseq, tyargs_taus); 
			val pats = generatePat(tyargs_taus, patseq)
			val _ = patseq_ref := pats (* !!! very important !!! SET THE pat list ref  *)
		in 
		  pats
		end
		else patseq
	)

	fun showMe(_, longcon, [], tau) = ()
	|	showMe(msg, longcon, pats, tau) =
	(
		(*print "\n";*)
		sayErr (msg^"["^Instrument.getPatAsString(
						Absyn.STRUCTpat(SOME longcon, pats, ref pats, Absyn.dummyInfo))^
			   "]=[");
		Ty.printType(tau);
		print "]\n"
	)

    (* Elaborate a pattern *)
    fun elab_pat_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, pat) =
      case pat
		of Absyn.WILDpat _ => (VE_pat, Ty.VAR(Ty.newTyvar()))
		| Absyn.LITpat(lit, _) => (VE_pat, elab_lit lit)
		| Absyn.CONpat(longcon, _) =>
			let val con_tau = fresh_longcon(ME, VE, longcon)
			val _ = assert_constant(con_tau, longcon)
			in
			  (VE_pat, con_tau)
			end
		| Absyn.STRUCTpat(SOME longcon, patseq, patseq_ref, ctxInfo) => (* TODO!! fix ref pats here *)
			let val con_tau = fresh_longcon(ME, VE, longcon)
			val pats = case con_tau of (* adrpo added *)
					   Ty.REL(tyarg_taus, _) => 
					   if containsNamed(patseq) 
					   then 
						let val x = fixPatSeq(patseq, patseq_ref, tyarg_taus)
						in 
						  (* showMe("STP -> ", longcon, x, con_tau); *)
						  x 
						end
					   else patseq   
					 | _ => patseq
			(*
			val (VE'_pat, args_taus) = elab_patseq_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, patseq)
			*)
			val (VE'_pat, args_taus) = 
				elab_patseq_os(
					relationId, 
					ctxInfoClause, 
					os, 
					ME, 
					VE, 
					VE_pat, 
					pats)			
			val res_tau = Ty.VAR(Ty.newTyvar())
			val _ = (debug "elab_pat_os.STRUCTpat\n"; Ty.unify(con_tau, Ty.REL(args_taus, [res_tau])))
				   (* adrpo added 2004-11-29 *)
				   handle exn =>
					((case exn
					of Ty.TypeError explain => 
						sayTyErr(explain,"while elaborating pattern", "", ctxInfo)
						| StaticElaborationError => () (* already explained *)
						| _ => strayExnBug(exn, "", ctxInfo));
					raise StaticElaborationError)			        
			in
			  (VE'_pat, res_tau)
			end
		(* TODO!! fix ref pats here! Actually there shouln't be any NAMED patterns here as this is just a tuple *)
		| Absyn.STRUCTpat(NONE, patseq, patseq_ref, _) => 
			let val (VE'_pat, args_taus) = 
			          elab_patseq_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, patseq)
			in
			  (VE'_pat, Ty.TUPLE(args_taus))
			end
		| Absyn.BINDpat(var, pat, _) =>
			let val (VE'_pat, tau) = 
			          elab_pat_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, pat)
			val _ = checkVar(VE, VE'_pat, var)
			val sigma = TyScheme.genNone tau
			val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=false}
			in
			 (IdentDict.insert(VE'_pat,var,bnd), tau)
			end
		| Absyn.IDENTpat(id, r, _) =>
			(* If it's bound in VE_pat, that's an error.
			* If it's bound as a CON in VE_init+VE, then it's a constant.
			* Otherwise enter it as a freshly bound variable in VE_pat.
			*)
			let fun mkbinding() =
			let val _ = 
					r := Absyn.BINDpat(id, 
						Absyn.WILDpat(Absyn.dummyInfo), 
						Absyn.dummyInfo)
				val tau = Ty.VAR(Ty.newTyvar())
				val sigma = TyScheme.genNone tau
				val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=false}
			in
				(IdentDict.insert(VE_pat,id,bnd), tau)
			end
			fun bound(StatObj.VALSTR{vk=StatObj.CON,sigma,...}) =
				let val longcon = Absyn.LONGID(NONE, id, Absyn.dummyInfo)
				val _ = r := Absyn.CONpat (longcon, Absyn.dummyInfo)
				val con_tau = TyScheme.instFree sigma
				val _ = assert_constant(con_tau, longcon)
				in
				  (VE_pat, con_tau)
				end
			| bound _ = mkbinding()
			val _ = checkVarNotBound(VE_pat, id)
			in
			case IdentDict.find(VE, id)
			of SOME valstr => bound valstr
			| NONE =>
				case IdentDict.find(StatObj.VE_init, id)
				of SOME valstr => bound valstr
				| NONE => mkbinding()
			end
		| Absyn.NAMEDpat(var, pat, _) =>
		    (* this is a named argument in pattern *)
			let val (VE'_pat, tau) = 
			          elab_pat_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, pat)
			(* TODO!! maybe is a good idea to bind the named argument in the relation 
			          something like na1 = na1 as ...
			val _ = checkVar(VE, VE'_pat, var)
			val sigma = TyScheme.genNone tau
			val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=false}
			*)
			in
			 (VE'_pat, Ty.NAMED(Absyn.identName var, tau))
			end

    and elab_patseq_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, patseq) =
      let fun loop([], VE_pat, rev_taus) = (VE_pat, rev rev_taus)
	    | loop(pat::patseq, VE_pat, rev_taus) =
		let val (VE_pat, tau) = 
		         elab_pat_os(relationId, ctxInfoClause, os, ME, VE, VE_pat, pat)
		in
		  loop(patseq, VE_pat, tau::rev_taus)
		end
      in
		loop(patseq, VE_pat, [])
      end

    (* Assert that an id is bound as a variable; return type scheme *)

    fun assert_var(id, StatObj.VALSTR{vk,sigma,...}) =
      case vk
		of StatObj.CON => idError("not a variable: ",
		                          Absyn.identName id, 
		                          Absyn.identCtxInfo id)
		| _ => sigma

    fun lookup_longvar(ME, VE, longvar as Absyn.LONGID(_, var, _)) =
      assert_var(var, lookup_longid(ME, VE, longvar))

    (* Elaborate an expression.
     * Disambiguate `IDENTexp's. Compute principal type.
     *)

    fun elab_exp(ME, VE, exp) =
      case exp
	of Absyn.LITexp(lit, _)		=> elab_lit lit
	 | Absyn.CONexp(longcon, _)	=>
	    let val con_tau = fresh_longcon(ME, VE, longcon)
		val _ = assert_constant(con_tau, longcon)
	    in
	      con_tau
	    end
	 | Absyn.VARexp(_, _)		=> bug("elab_exp: VARexp")
	 | Absyn.IDENTexp(longid, r, ctxInfo)	=>
	    let val StatObj.VALSTR{vk,sigma,...} = lookup_longid(ME, VE, longid)
		val tau = TyScheme.instFree sigma
		(* val _ = print ("\nIDENTexp("^Absyn.lidentName longid^")") *)
		val _ = case vk
			  of StatObj.CON =>
			      (r := Absyn.CONexp(longid, ctxInfo);
			       assert_constant(tau, longid))
			   | _ => r := Absyn.VARexp(longid, ctxInfo)
	    in
	      tau
	    end
	 | Absyn.STRUCTexp(SOME longcon, expseq, ctxInfo)	=>
	    let val con_tau = fresh_longcon(ME, VE, longcon)
		val args_taus = elab_expseq(ME, VE, expseq)
		val res_tau = Ty.VAR(Ty.newTyvar())
		val _ = (debug "elab_exp.STRUCTexp\n"; Ty.unify(con_tau, Ty.REL(args_taus, [res_tau])))
				   (* adrpo added 2004-11-29 *)
				   handle exn =>
					((case exn
					of Ty.TypeError explain => 
						sayTyErr(explain,"while elaborating expression", "", ctxInfo)
						| StaticElaborationError => () (* already explained *)
						| _ => strayExnBug(exn, "", ctxInfo));
					raise StaticElaborationError)
		in
	      res_tau
	    end
	 | Absyn.STRUCTexp(NONE, expseq, _)		=>
	    let val args_taus = elab_expseq(ME, VE, expseq)
	    in
	      Ty.TUPLE(args_taus)
	    end

    and elab_expseq(ME, VE, expseq) =
      map (fn exp => elab_exp(ME, VE, exp)) expseq

    fun elab_exp_os(relationId, ctxInfoClause, os, ME, VE, exp) =
    case exp
	of Absyn.LITexp(lit, _)		=> elab_lit lit
	 | Absyn.CONexp(longcon, _)	=>
	    let val con_tau = fresh_longcon(ME, VE, longcon)
		val _ = assert_constant(con_tau, longcon)
	    in
	      con_tau
	    end
	 | Absyn.VARexp(_, _)		=> bug("elab_exp: VARexp")
	 | Absyn.IDENTexp(longid, r, ctxInfo)	=>
	    let val StatObj.VALSTR{vk,sigma,...} = lookup_longid(ME, VE, longid)
		val tau = TyScheme.instFree sigma
		(*val _ = printVarLong(relationId, ctxInfoClause, os, longid, tau) *)
		val _ = case vk
			  of StatObj.CON =>
			      (r := Absyn.CONexp(longid, ctxInfo);
			       assert_constant(tau, longid))
			   | _ => r := Absyn.VARexp(longid, ctxInfo)
	    in
	      tau
	    end
	 | Absyn.STRUCTexp(SOME longcon, expseq, ctxInfo)	=>
	    let val con_tau = fresh_longcon(ME, VE, longcon)
		val args_taus = elab_expseq_os(relationId, ctxInfoClause, os, ME, VE, expseq)
		val res_tau = Ty.VAR(Ty.newTyvar())
		val _ = (debug "elab_exp_os.STRUCTexp\n"; Ty.unify(con_tau, Ty.REL(args_taus, [res_tau])))
				   (* adrpo added 2004-11-29 *)
				   handle exn =>
					((case exn
					of Ty.TypeError explain => 
						sayTyErr(explain,"while elaborating expression", "", ctxInfo)
						| StaticElaborationError => () (* already explained *)
						| _ => strayExnBug(exn, "", ctxInfo));
					raise StaticElaborationError)
		in
	      res_tau
	    end
	 | Absyn.STRUCTexp(NONE, expseq, _)	=>
	    let val args_taus = elab_expseq_os(relationId, ctxInfoClause, os, ME, VE, expseq)
	    in
	      Ty.TUPLE(args_taus)
	    end

    and elab_expseq_os(relationId, ctxInfoClause, os, ME, VE, expseq) =
      map (fn exp => elab_exp_os(relationId, ctxInfoClause, os, ME, VE, exp)) expseq

	(*
    fun checkLocalVars(TE_dec, VE_localVars, VE_inferedVars, localVars) =
      let fun checkValue(kind, var_spec, sigma_spec) =
	    case IdentDict.find'(VE_inferedVars, var_spec)
	      of NONE => idError("specified "^kind^" not defined: ", 
							Absyn.identName var_spec, Absyn.identCtxInfo var_spec)
	       | SOME(var_dec, StatObj.VALSTR{sigma=sigma_dec,...}) =>
		  let val tau_spec = TyScheme.instRigid sigma_spec
		      val tau_dec = TyScheme.instFree sigma_dec
		  in
		    (debug "check_specs.checkValue\n"; Ty.unify(tau_dec, tau_spec))
		    handle Ty.TypeError explain =>
			    (sayTyErr(explain, "the specification for", Absyn.identName var_spec, Absyn.identCtxInfo var_spec);
			     sayIdError("the actual type of ", Absyn.identName var_dec, Absyn.identCtxInfo var_dec);
			     idError("does not match its specification: ", Absyn.identName var_spec, Absyn.identCtxInfo var_spec))
			 | exn => strayExnBug(exn, Absyn.identName var_spec, Absyn.identCtxInfo var_spec)
		  end
	  fun check((var, SOME(ty), exp, attr)) =
		checkValue("variable", var, assert_var(var, lookupVar(VE_localVars, var)))
      in
		List.app check localVars
      end
   *)

    (* Elaborate a goal, return updated VE *)

    fun elab_goal(relationId, ctxInfoClause, os, ME, VE, goal) =
    case goal
	of Absyn.CALLgoal(longvar, expseq, patseq, patseq_ref, ctxInfo) => (* TODO!! fix ref pats here *)
	   let val rel_sigma = lookup_longvar(ME, VE, longvar)
		   (* val _ = printVarLong(relationId, ctxInfoClause, os, longvar, TyScheme.instFree rel_sigma) *)
		   (*
		   val pats = case TyScheme.instFree rel_sigma of
					   Ty.REL(_, tyargs_taus) => fixPatSeq(patseq, patseq_ref, tyargs_taus) (* adrpo added *)
					 | _ => patseq
		   val _ = if containsNamed(patseq) 
			        then showMe("CAL -> ",longvar, pats, TyScheme.instFree rel_sigma) 
			        else ()
		   *)
		   val exp_taus = elab_expseq_os(relationId, ctxInfoClause, os, ME, VE, expseq)
		   
		   val (VE_pat,pat_taus) = 
				elab_patseq_os(relationId, ctxInfoClause, os, ME, VE, IdentDict.empty, patseq)
		   (*
		   val (VE_pat,pat_taus) = 
				elab_patseq_os(relationId, ctxInfoClause, os, ME, VE, IdentDict.empty, pats)
		   *)
    	   val _ = (debug "elab_goal.CALLgoal\n"; (Ty.unify(TyScheme.instFree rel_sigma, Ty.REL(exp_taus, pat_taus)))) 
				   (* adrpo added 2004-11-29 *)
				   handle exn =>
					((case exn
					of Ty.TypeError explain => 
						sayTyErr(explain,"this CALL goal: ", Absyn.lidentName longvar, ctxInfo)
						| StaticElaborationError => () (* already explained *)
						| _ => strayExnBug(exn, Absyn.lidentName longvar, ctxInfo));
					raise StaticElaborationError)
	   in
	    IdentDict.plus(VE, VE_pat)
	   end
	 | Absyn.EQUALgoal(var, exp, ctxInfo) =>
	    let val tau = elab_exp_os(relationId, ctxInfoClause, os, ME, VE, exp)
	    in
	      case IdentDict.find(VE, var)
			of NONE =>
				if !Control.allowImplicitLet then
				 let val sigma = TyScheme.genNone tau
				  val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=false}
				 in
					IdentDict.insert(VE, var, bnd)
				 end
				else 
				 idUnboundError("variable", var)
			| SOME(StatObj.VALSTR{sigma,...}) =>
				let val _ = 
				           ( debug "elab_goal.EQUALgoal\n"; 
				           (
				             Ty.unify(TyScheme.instFree sigma, tau),
				             Ty.mustAdmitEq tau
				            )
				            )
				           (* adrpo added 2004-11-29 *)
							handle exn =>
							((case exn
							of Ty.TypeError explain => 
									sayTyErr(explain,"this EQUAL goal: ", Absyn.identName var, ctxInfo)
								| StaticElaborationError => () (* already explained *)
								| _ => strayExnBug(exn, Absyn.identName var, ctxInfo));
							raise StaticElaborationError)				
				in
				  VE
				end
	    end 
	 | Absyn.LETgoal(pat, exp, ctxInfo) => 
	    let val tau = elab_exp_os(relationId, ctxInfoClause, os, ME, VE, exp)
			val (VE_pat, tau') = elab_pat_os(relationId, ctxInfoClause, os, ME, VE, IdentDict.empty, pat)
			val _ = (debug "elab_goal.LETgoal\n"; Ty.unify(tau, tau'))
			    (* adrpo added 2004-11-29 *)
				handle exn =>
				((case exn
				of Ty.TypeError explain => sayTyErr(explain,"this LET goal: ", "", ctxInfo)
				| StaticElaborationError => () (* already explained *)
				| _ => strayExnBug(exn, "", ctxInfo));
				raise StaticElaborationError)
	    in
	      IdentDict.plus(VE, VE_pat)
	    end 
	 | Absyn.NOTgoal(goal, _) => (elab_goal(relationId, ctxInfoClause, os, ME, VE, goal); VE)
	 | Absyn.ANDgoal(g1,g2, _) => elab_goal(relationId, ctxInfoClause, os, ME, 
									elab_goal(relationId, ctxInfoClause, os, ME, VE, g1), g2)
	 | Absyn.CONDgoal(g1,g2,g3,_) => 
		elab_goal(relationId, ctxInfoClause, os, ME, 
		elab_goal(relationId, ctxInfoClause, os, ME, 
		elab_goal(relationId, ctxInfoClause, os, ME, VE, g1), g2), g3)

    fun elab_goal_opt(relationId, ctxInfoClause, os, ME, VE, SOME goal) = 
			elab_goal(relationId, ctxInfoClause, os, ME, VE, goal)
    |   elab_goal_opt(relationId, ctxInfoClause, os, ME, VE, NONE) = VE

    (* Check that a set of clauses conform with the given type *)

    fun checkClauseName(var_rel, var_clause) =
      if Absyn.identEqual(var_rel, var_clause) then ()
      else (sayIdError("the name in the clause is: ", 
						Absyn.identName var_clause, Absyn.identCtxInfo var_clause);
	    idError("but the name of the relation is: ", 
	             Absyn.identName var_rel, Absyn.identCtxInfo var_rel))

    fun mkRanTaus(tau, clause) =
      let fun resultAry(Absyn.RETURN (exps, _)) = List.length exps
	    | resultAry(Absyn.FAIL(_)) = ~1
	  fun clauseRanAry(Absyn.CLAUSE1(_, _, _, result, _, _, _)) = resultAry result
	    | clauseRanAry(Absyn.CLAUSE2(cl1, cl2, _)) =
		case clauseRanAry cl1
		  of ~1 => clauseRanAry cl2
		   | ary => ary
	  fun loop(n, rest) =
	    if n < 1 then rest else loop(n-1, Ty.VAR(Ty.newTyvar())::rest)
      in
		case Ty.deref tau
		of Ty.REL(_,ranTaus) => ranTaus
		| _ => loop(clauseRanAry clause, [])
		end

    (* Closing a variable environment *)
    fun Close_VE VE =
      let fun Close_bnd(var, StatObj.VALSTR{sigma,...}, VE) =
	    (* This ought be instRigid, but that won't work here because
	     * most sigmas aren't constructed from explicit types.
	     * Since the sigma is instantiated only to be re-generalized,
	     * instFree will work just fine.
	     *)
	    let val tau = TyScheme.instFree sigma
		val sigma' = TyScheme.genAll tau
		val bnd = StatObj.VALSTR{vk=StatObj.REL, sigma=sigma', localVE=IdentDict.empty, global=false}
	    in
	      IdentDict.insert(VE, var, bnd)
	    end
      in
		IdentDict.fold(Close_bnd, IdentDict.empty, VE)
      end


    fun checkClause(os, modid, ME, VE, TE, varRel, tau, clause) =
    let 
      val defaultRanTaus = mkRanTaus(tau, clause)
      (*val ruleNr = ref 0*)
	  fun elabResult(relationId, ctxInfoClause, os, ME, VE, Absyn.RETURN (exps, _)) = 
			elab_expseq_os(relationId, ctxInfoClause, os, ME, VE, exps)
	    | elabResult(_, _, _, _, _, Absyn.FAIL(_)) = defaultRanTaus
	  fun check(Absyn.CLAUSE1(goal_opt, var, patseq, result, patseq_ref, localVars, ctxInfo)) = 
		(
		let (*val _ = ruleNr := !ruleNr + 1*)
			fun elaborateLocalVariables([], d) = d
			|	elaborateLocalVariables((id, SOME(ty), _, Absyn.ATTRIBUTES{output,...})::rest, d) =
				let val tauLocal = 
						SOME(elab_ty ME TE NONE ty)
						handle exn => (case exn of _ => NONE)
				in
				case tauLocal of
					SOME(tauLocal) => 
					let val sigmaLocal = TyScheme.genAll tauLocal
						val bndLocal = StatObj.VALSTR{vk=StatObj.REL, sigma=sigmaLocal, localVE=IdentDict.empty, global=false}
					in
						elaborateLocalVariables(rest, IdentDict.insert(d, id, bndLocal))
					end
				|	NONE => elaborateLocalVariables(rest, d)
				end
			|	elaborateLocalVariables(_::rest, d) = elaborateLocalVariables(rest, d)
			val StatObj.VALSTR{localVE,...} = lookupVar(VE, varRel) 
			val localVE = elaborateLocalVariables(localVars, localVE)
			(*val pats = case tau of
					   Ty.REL(tyargs_taus, _) => fixPatSeq(patseq, patseq_ref, tyargs_taus) (* adrpo added *)
					 | _ => patseq
			   val _ = if containsNamed(patseq) 
			        then showMe("CLA -> ",Absyn.LONGID(NONE, var, ctxInfo), pats, tau)
			        else ()
			 *)
			 (* DO NOT ADD THE LOCAL VARIABLE DEFINITION TO THE ENVIRONMENT
			 val VE = IdentDict.plus(VE,localVE) 
			 *)
			 val (VE_pat, domTaus) = elab_patseq_os(varRel, ctxInfo, os, ME, VE, IdentDict.empty, patseq)
			 (*
			 val (VE_pat, domTaus) = elab_patseq_os(varRel, ctxInfo, os, ME, VE, IdentDict.empty, pats)
			 *)
		     val VE' = elab_goal_opt(varRel, ctxInfo, os, ME, IdentDict.plus(VE,VE_pat), goal_opt)
		     
		     val ranTaus = elabResult(varRel, ctxInfo, os, ME, VE', result)
		     val _ = (debug "checkClause.CLAUSE1\n"; Ty.unify(tau, Ty.REL(domTaus, ranTaus)))
		     val _ = checkClauseName(varRel, var)
		     
		  fun printVE(var, StatObj.VALSTR{sigma,vk,...}, Dict) =
			let val tau = TyScheme.instFree sigma
			val sigma' = TyScheme.genAll tau
			val Absyn.INFO(sp, ep) = Absyn.identCtxInfo var
			val (file, sl, sc, el, ec) = getLoc(sp, ep)
			val Absyn.INFO(sp_c, ep_c) = ctxInfo
			val (_, sl_c, sc_c, el_c, ec_c) = getLoc(sp_c, ep_c)
		  in
		    (case vk of 
		     StatObj.VAR => 
				(
				 printOs (os, "v:<");
				 printOs (os, file); 
				 printOs (os, ">:"); 
				 printOs (os, (Int.toString sl)^"."^(Int.toString sc)^"."^(Int.toString el)^"."^(Int.toString ec)^"|[");
				 printOs (os, (Int.toString sl_c)^"."^(Int.toString sc_c)^"."^(Int.toString el_c)^"."^(Int.toString ec_c)^"]|");
				 printOs (os, Absyn.identName varRel);
				 printOs (os, "[");
				 printOs (os, Absyn.identName var);
				 printOs (os, ":"); 
				 
				 if !Control.qualifiedRdb 
				 then Ty.printTypeOs(os, "", tau)
				 else Ty.printTypeOs(os, Absyn.identName modid, tau);
				 
				 printOs (os, "]\n"))
			 | _ => ());
			Dict
		  end
		  
		  (* only the non global ones *)
		  fun checkLocalVariable(var, StatObj.VALSTR{sigma,vk,global,...}, d) =
		  if (String.isPrefix "temp_t_"  (Absyn.identName var)) then (d)
		  else  if global then (d)
		  else
		  let val tau = TyScheme.instFree sigma
				val sigma' = TyScheme.genAll tau
				val tau = TyScheme.instFree sigma			
		  in
		    case vk of 
				StatObj.VAR => 
				let val _ = debug ("checkLocalVariable: relation["^(Absyn.identName varRel)^"]\n")
					val tauLocal = 
							case lookupDeclaredVar(localVE, var) of
								StatObj.VALSTR{sigma,vk,...} =>	SOME(TyScheme.instFree sigma) 
							(* |	_ => NONE *)
				in
					case tauLocal of 
						SOME(tLocal) => 
						(
						if debugFlag
						then (
						print ("variable:["^(Absyn.identName var)^"]");
						print (" - I["); Ty.printType tau; 
						print ("] ~ D["); Ty.printType tLocal; print "]\n"
						)
						else ();
						 
						(Ty.unify(tLocal, tau)) 
						handle exn =>
						(
						case exn of 
							Ty.TypeError explain => 
							(   
								sayIdError(
									"the declared variable type does not match the use context: ", 
									Absyn.identName var,
									Absyn.identCtxInfo var );
								sayErr("It's declared type:\n"); 
								sayTyErrExplain(explain); sayErr("\n")
							)
						|	StaticElaborationError => () (* already explained *)
						|	_ => 
							strayExnBug(exn, Absyn.identName var, ctxInfo);
						raise StaticElaborationError
						)
						)
					|	_ => ()
				end
		   |	_ => ();
		   d
		  end
		  (* checking of locals only if we compile from .mo *)
		  val _ = case !Control.currentlyCompiling of
					Control.MO_FILE => IdentDict.fold(checkLocalVariable, IdentDict.empty, VE')
				  | _ => (VE')
		 in
		   case os of
		     SOME (_) => (IdentDict.fold(printVE, IdentDict.empty, VE'); ())
		   | NONE => ()
		 end handle exn =>
		   ((case exn
		       of Ty.TypeError explain => 
				sayTyErr(explain,"this clause: ", Absyn.identName var, ctxInfo)
				| StaticElaborationError => () (* already explained *)
				| _ => strayExnBug(exn, Absyn.identName var, ctxInfo));
		    raise StaticElaborationError))
	    | check(Absyn.CLAUSE2(cl1, cl2, _)) = (check cl1; check cl2)
      in
		check clause
      end

    (* Check a set of relation bindings *)

    fun check_relbinds(os, modid, ME, TE, VE, relbinds) =
      let fun check(Absyn.RELBIND(var, _, clause, localVars, matchExps, _)) =
	    let val sigma = assert_rel(var, lookupVar(VE, var))
		val tau = TyScheme.instRigid sigma
	    in
	      checkClause(os, modid, ME, VE, TE, var, tau, clause)
	    end
      in
		List.app check relbinds
      end

    (* Elaborate a set of relation bindings *)
    fun elab_relbinds(ME, TE, VE, VE_rel, relbinds) =
      let fun elab(Absyn.RELBIND(var, ty_opt, _, localVars, matchExps, _), VE_rel) =
	    (* Here we should do checkVar(empty, VE+VE_rel, var), but to
	     * avoid the IdentDict.plus, we inline the equivalent checks.
	     *)
	    let (* val _ = print ("elab_relbinds: "^(Absyn.identName var)^"\n") *)
	        (* maybe bind the relation with the type from the specs then unify with the existing type *)
	        (* case IdentDict.find'(env, id) of NONE => () | SOME(id',_) => idRebindError(kind, id, id') *)
	    val _ = checkVarNotBound(VE_rel, var)
		val _ = checkVarNotBound(VE, var)
		val _ = checkNotCon(StatObj.sourceInit, StatObj.VE_init, var)
		val tau =
		  case ty_opt
		    of SOME ty => elab_ty ME TE NONE ty
		     | NONE => Ty.VAR(Ty.newTyvar())
		val sigma = TyScheme.genNone tau
		fun elaborateLocalVariables([], d) = d
		|	elaborateLocalVariables((id, SOME(ty), _, _)::rest, d) =
			let val tauLocal = elab_ty ME TE NONE ty
				val sigmaLocal = TyScheme.genAll tauLocal
				val bndLocal = StatObj.VALSTR{vk=StatObj.REL, sigma=sigmaLocal, localVE=IdentDict.empty, global=false}
			in
				elaborateLocalVariables(rest, IdentDict.insert(d, id, bndLocal))				
			end
		|	elaborateLocalVariables(_::rest, d) = elaborateLocalVariables(rest, d)
		
		val bnd = StatObj.VALSTR{
					vk=StatObj.REL, 
					sigma=sigma, 
					localVE=elaborateLocalVariables(localVars, IdentDict.empty),
					global=false}
	    in
	      IdentDict.insert(VE_rel, var, bnd) 
	    end
      in
		List.foldl elab VE_rel relbinds
      end


    (* Elaborate a module's declarations *)

    fun elab_decs(os, ME, TE, VE, modid, decs) =
      let
        val _ = debug("elab_decs\n") 
        fun elab(ME, TE, VE, []) = (ME,TE,VE)
	    | elab(ME, TE, VE, Absyn.WITHdec(_, interface, _)::decs) =
		let val (_,TE',VE',modid') = elab_interface(ME, !interface)
		    val Absyn.INTERFACE({source=source',...}, _) = !interface
		    val modstr = StatObj.MODSTR{TE=TE', VE=VE', source=source'}
		    val ME' = IdentDict.insert(ME, modid', modstr)
		in
		  elab(ME', TE, VE, decs)
		end
	    | elab(ME, TE, VE, Absyn.TYPEdec(typbinds, _)::decs) =
		let val TE' = elab_typbinds(ME, IdentDict.empty, TE, typbinds)
		in
		  elab(ME, TE', VE, decs)
		end
	    | elab(ME, TE, VE, Absyn.DATAdec(datbinds, withbind, _)::decs) =
		let val (TE',VE') = elab_datatype(ME,TE,VE,modid,datbinds,withbind)
		in
		  elab(ME, TE', VE', decs)
		end
	    | elab(ME, TE, VE, Absyn.VALdec(var, exp, ctxInfo)::decs) =
		let val _ = checkVar(IdentDict.empty, VE, var)
		    val tau = elab_exp(ME, VE, exp)
		      handle exn =>
			((case exn
			    of Ty.TypeError explain => 
					sayTyErr(explain, "this declaration:", Absyn.identName var, ctxInfo)
			     | StaticElaborationError => ()
			     | _ => strayExnBug(exn, Absyn.identName var, ctxInfo));
			 raise StaticElaborationError)
		    val sigma = TyScheme.genAll tau
		    val bnd = StatObj.VALSTR{vk=StatObj.VAR, sigma=sigma, localVE=IdentDict.empty, global=true}
		    val VE' = IdentDict.insert(VE, var, bnd)
		in
		  elab(ME, TE, VE', decs)
		end
	    | elab(ME, TE, VE, Absyn.RELdec(relbinds, _)::decs) =
		let val VE_rel = elab_relbinds(ME, TE, VE, IdentDict.empty, relbinds)
		    val _ = 
				check_relbinds(
					os, 
					modid, 
					ME, 
					TE, 
					IdentDict.plus(VE,VE_rel), 
					relbinds)
		    val VE'_rel = Close_VE VE_rel
		in
		  elab(ME, TE, IdentDict.plus(VE,VE'_rel), decs)
		end
      in
		elab(ME, TE, VE, decs)
      end

    (* Check a module's interface specifications *)

    fun check_nonabstract(false, _) = ()
      | check_nonabstract(true, tycon) =
	  idError("abstract type not completed: ", 
			   Absyn.identName tycon, Absyn.identCtxInfo tycon)

    fun check_arity(theta, tyvarseq, tycon) =
      if TyFcn.arity theta = length tyvarseq then ()
      else idError("abstract type implemented with wrong arity: ", 
					Absyn.identName tycon, Absyn.identCtxInfo tycon)

    fun check_equality(theta, tycon) =
      if TyFcn.admitsEq theta then ()
      else idError("abstract eqtype implemented without equality: ", 
					Absyn.identName tycon, Absyn.identCtxInfo tycon)

    fun check_specs(TE_dec, VE_spec, VE_dec, specs) =
      let fun checkValue(kind, var_spec, sigma_spec) =
	    case IdentDict.find'(VE_dec, var_spec)
	      of NONE => idError("specified "^kind^" not defined: ", 
							Absyn.identName var_spec, Absyn.identCtxInfo var_spec)
	       | SOME(var_dec, StatObj.VALSTR{sigma=sigma_dec,...}) =>
		  let val tau_spec = TyScheme.instRigid sigma_spec
		      val tau_dec = TyScheme.instFree sigma_dec
		  in
		    (debug "check_specs.checkValue\n"; Ty.unify(tau_dec, tau_spec))
		    handle Ty.TypeError explain =>
			    (sayTyErr(explain, "the specification for", Absyn.identName var_spec, Absyn.identCtxInfo var_spec);
			     sayIdError("the actual type of ", Absyn.identName var_dec, Absyn.identCtxInfo var_dec);
			     idError("does not match its specification: ", Absyn.identName var_spec, Absyn.identCtxInfo var_spec))
			 | exn => strayExnBug(exn, Absyn.identName var_spec, Absyn.identCtxInfo var_spec)
		  end
	  fun check(Absyn.VALspec(var, _, _)) =
		checkValue("variable", var,
			   assert_var(var, lookupVar(VE_spec, var)))
	    | check(Absyn.RELspec(var, _, _)) =
		checkValue("relation", var,
			   assert_rel(var, lookupVar(VE_spec, var)))
	    | check(Absyn.ABSTYPEspec(eq, tyvarseq, tycon, _)) =
		let val StatObj.TYSTR{theta,abstract} = lookupTycon(TE_dec, tycon)
		    val _ = check_nonabstract(abstract, tycon)
		    val _ = check_arity(theta, tyvarseq, tycon)
		    val _ = if eq then check_equality(theta, tycon) else ()
		in
		  ()
		end
	    | check(_) = ()
      in
		List.app check specs
      end

      fun onlycon(var, bnd as StatObj.VALSTR{vk=StatObj.CON,...}, VE') = IdentDict.insert(VE', var, bnd)
	    | onlycon(_, _, VE') = VE'      
		(* maybe have the relation from specs also inserted in the VE  
		| onlycon(var, bnd as StatObj.VALSTR{vk=StatObj.REL,...}, VE') = IdentDict.insert(VE', var, bnd)
		*)
	  
	  val Absyn.MODULE(main_interface, decs, _) = main_module
	  val Absyn.INTERFACE({specs,source,...}, _) = main_interface
	  
	  fun elab() =
	    let val _ = annotate_module(main_module)
		val (ME,TE,VE,modid) = elab_interface(StatObj.ME_init, main_interface) (* elaborates the specifications *)
		val VE' = IdentDict.fold(onlycon, IdentDict.empty, VE)
		val (ME',TE',VE'') = elab_decs(os, ME, TE, VE', modid, decs)
		val _ = check_specs(TE', VE, VE'', specs)
	    in
	      (modid, ME, TE, VE, ME', TE', VE', VE'')
	    end
      in
		withSource(source, elab)
      end

    (* Check a separately compiled module *)	
    fun checkModule(os, (module as Absyn.MODULE(interface as Absyn.INTERFACE({source,...}, _), _, _), repository)) =
      let val _ = debug("checkModule\n")
          val alreadyDumped = ref []
		  val (modid, ME, TE, VE, ME', TE', VE', VE'') = elab_module(os, module, repository)
          val _ = currentSource := source
		  
		  fun printVE(var, StatObj.VALSTR{sigma,vk,...}, Dict) =
			let val tau = TyScheme.instFree sigma
			val sigma' = TyScheme.genAll tau
			val Absyn.INFO(sp, ep) = Absyn.identCtxInfo var
			val (file, sl, sc, el, ec) = getLoc(sp, ep)			
		  in
		    case vk of 
		     StatObj.REL => 
		     (  
		        printOs(os, "r:<");
				printOs (os, file); 
				printOs (os, ">:"); 
				printOs (os, (Int.toString sl)^"."^(Int.toString sc)^"."^(Int.toString el)^"."^(Int.toString ec)^"|"); 
							
				if !Control.qualifiedRdb 
				then (printOs(os, Absyn.identName modid); printOs(os, "."))
				else ();
				
				printOs(os,Absyn.identName var); 
				printOs(os,":");
				
				if !Control.qualifiedRdb 
				then Ty.printTypeOs(os, "", tau)
				else Ty.printTypeOs(os, Absyn.identName modid, tau);
				
				printOs(os,"\n")
		     )
		     | StatObj.CON => 
		     (
				printOs (os, "c:<");
				printOs (os, file); 
				printOs (os, ">:"); 
				printOs (os, (Int.toString sl)^"."^
							(Int.toString sc)^"."^
							(Int.toString el)^"."^
							(Int.toString ec)^"|"); 
							
				if !Control.qualifiedRdb 
				then (printOs (os, Absyn.identName modid); printOs(os, "."))
				else ();

				printOs(os, Absyn.identName var); 
				printOs(os,":");
				
				if !Control.qualifiedRdb 
				then Ty.printTypeOs(os, "", tau)
				else Ty.printTypeOs(os, Absyn.identName modid, tau);
				
				printOs(os,"\n")
			 )
		     | StatObj.VAR => 
		     (  
		        printOs(os, "l:<");
				printOs (os, file); 
				printOs (os, ">:"); 
				printOs (os, (Int.toString sl)^"."^
							 (Int.toString sc)^"."^
							 (Int.toString el)^"."^
							 (Int.toString ec)^"|"); 
							
				if !Control.qualifiedRdb 
				then (printOs(os, Absyn.identName modid); printOs(os, "."))
				else ();
				
				printOs(os,Absyn.identName var); 
				printOs(os,":");
				
				if !Control.qualifiedRdb 
				then Ty.printTypeOs(os, "", tau)
				else Ty.printTypeOs(os, Absyn.identName modid, tau);
				
				printOs(os,"\n")
		     );
			Dict
		  end
		  fun printTE(tycon, StatObj.TYSTR{theta, ...}, Dict) =
		  let val Absyn.INFO(sp,ep) = Absyn.identCtxInfo tycon
			  val (file, sl, sc, el, ec) = getLoc(sp, ep)
		  in
		    printOs (os, "t:<");
			printOs (os, file); 
			printOs (os, ">:"); 
			printOs (os, (Int.toString sl)^"."^
				         (Int.toString sc)^"."^
				         (Int.toString el)^"."^
				         (Int.toString ec)^"|"); 		  
			
			if !Control.qualifiedRdb 
			then (printOs (os, Absyn.identName modid); printOs(os, "."))
			else ();
			
			printOs (os, Absyn.identName tycon);
			(*printOs (os, "["^(Int.toString(bvars))^"]");*) 
			printOs (os, "\n");
			Dict
		  end		  
		  fun printDict(mod_id, StatObj.MODSTR{TE=M_TE, VE=M_VE,...}, Dict) =
		  let val Absyn.INFO(sp, ep) = Absyn.identCtxInfo mod_id
              val (file, sl, sc, el, ec) = getLoc(sp, ep)
		      			
			  fun is_there(str) = (str = (Absyn.identName mod_id))
			  
			  fun printVE_CON_VAL(var, StatObj.VALSTR{sigma,vk,...}, Dict) =
					let val tau = TyScheme.instFree sigma
					val sigma' = TyScheme.genAll tau
					val Absyn.INFO(sp, ep) = Absyn.identCtxInfo var
					val (file, sl, sc, el, ec) = getLoc(sp, ep)
				in
					case vk of 
					StatObj.CON => 
					(
						printOs(os, "c:<");
						printOs (os, file); 
						printOs (os, ">:"); 
						printOs (os, (Int.toString sl)^"."^
									 (Int.toString sc)^"."^
									 (Int.toString el)^"."^
									 (Int.toString ec)^"|"); 
									
						printOs(os, Absyn.identName mod_id); 
						printOs(os, ".");
						printOs(os, Absyn.identName var); 
						printOs(os,":");
						
						Ty.printTypeOs(os, "", tau);
						
						printOs(os,"\n")
					)
					| StatObj.VAR => 
					(
						printOs(os, "l:<");
						printOs (os, file); 
						printOs (os, ">:"); 
						printOs (os, (Int.toString sl)^"."^
									(Int.toString sc)^"."^
									(Int.toString el)^"."^
									(Int.toString ec)^"|"); 
									
						printOs(os, Absyn.identName mod_id); printOs(os, ".");
						printOs(os,Absyn.identName var); 
						printOs(os,":");
						Ty.printTypeOs(os, "", tau);						
						printOs(os,"\n")
					)
					| StatObj.REL => 
					(  
						printOs(os, "r:<");
						printOs (os, file); 
						printOs (os, ">:"); 
						printOs (os, (Int.toString sl)^"."^
									(Int.toString sc)^"."^
									(Int.toString el)^"."^
									(Int.toString ec)^"|"); 
									
						printOs(os, Absyn.identName mod_id); printOs(os, ".");
						printOs(os,Absyn.identName var); 
						printOs(os,":");
						Ty.printTypeOs(os, "", tau);
						printOs(os,"\n")
					);
					Dict
				end		  
				
		  in
		    if ((Absyn.identName mod_id) <> "RML" andalso 
		        (Absyn.identName mod_id) <> (Absyn.identName modid) andalso 
		        not (List.exists is_there (!alreadyDumped)))
		    then
				( 
				printOs(os, "/*  - start external M_VE_CON: "^(Absyn.identName mod_id)^" */\n");
				IdentDict.fold(printVE_CON_VAL, IdentDict.empty, M_VE);
				printOs(os, "/* - end external */\n");
				(* add the dumped module to a list to not dump it again *)
				alreadyDumped := (Absyn.identName mod_id)::(!alreadyDumped);
				()
				)
			else
				();
			Dict
		  end		  		  
      in
        case os of
         SOME(_) => 
         (
			if !Control.rdbOnly
			then 
			(
				printOs(os, "/*  ME Dict: */\n"); 
				IdentDict.fold(printDict, IdentDict.empty, ME);
				printOs(os, "/*  ME' Dict: */\n");			
				IdentDict.fold(printDict, IdentDict.empty, ME');								
				()
			)
			else ();
			printOs(os, "/*  TE': */\n");
			IdentDict.fold(printTE, IdentDict.empty, TE');
			printOs(os, "/*  VE'': */\n");
			IdentDict.fold(printVE, IdentDict.empty, VE'');
		 ())
		| NONE => ()
      end

    (* Elaborate a sequence of modules *)

    fun elab_modseq(os, ME, modseq, repository) =
      let fun elab(module, ME) =
	    let val (modid, _, _, VE, _, _, _, _) = elab_module(os, module, repository)
		val _ = assert_unbound_modid(ME, modid)
		val Absyn.MODULE(Absyn.INTERFACE({source,...}, _), _, _) = module
		val modstr = StatObj.MODSTR{TE=IdentDict.empty, VE=VE, source=source}
	    in
	      IdentDict.insert(ME, modid, modstr)
	    end
      in
		List.foldl elab ME modseq 
      end

    (* Check an entire program *)

    fun checkProgram(os, (modseq, repository)) =
      let val ME = elab_modseq(os, StatObj.ME_init, modseq, repository)
	  val VE = lookup_modid_VE(ME, Absyn.rmlIdent "Main")
	  val sigma = assert_rel(Absyn.rmlIdent "Main.main",
				 lookupVar(VE, Absyn.rmlIdent "main"))
	  val tau = Ty.CONS([StatObj.tau_string], StatObj.t_list)
      in
		(debug "checkProgram\n"; Ty.unify(TyScheme.instFree sigma, Ty.REL([tau],[])))
      end

  end (* functor StatElabFn *)
