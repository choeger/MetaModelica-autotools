(* reorder/reorder-ty.sml *)

functor ReorderTyFn(structure Util : UTIL
		    structure Absyn : ABSYN
		    structure ReorderSCC : REORDER_SCC
		    sharing type Absyn.IdentDict.Key.ord_key = Absyn.ident
		      ) : REORDER_TY =
  struct

    structure Absyn = Absyn
    structure IdentDict = Absyn.IdentDict

    fun bug s = Util.bug("ReorderTy."^s)

    (* DATA TYPES *)

    datatype tybnd	= TYPBND of Absyn.typbind
			| DATBND of Absyn.datbind

    datatype tydec	= TYPDEC of Absyn.typbind
			| DATDEC of Absyn.datbind list * Absyn.typbind list

    (* ERROR MESSAGES *)

    fun sayMsg msg = TextIO.output(TextIO.stdErr, msg)

    fun sayIdError(source, msg, Absyn.IDENT(name, Absyn.INFO(left, right))) =
      Absyn.Source.sayMsg source ("Error: "^msg^name, left, right)

    exception ReorderTypeDeclarationsError

    fun idError(source, msg, id) =
      (sayIdError(source, msg, id);
       raise ReorderTypeDeclarationsError)

    (* DEPENDENCY ANALYSIS *)

    fun depsOfTy TE =
      let fun depsOf(ty, deps) =
	    case ty
	      of Absyn.VARty (_, _) => deps
	       | Absyn.CONSty(tyseq, Absyn.LONGID(modid_opt,tycon, _), _) =>
		  let val deps = List.foldl depsOf deps tyseq
		  in
		    case modid_opt
		      of SOME _ => deps	(* non-local *)
		       | NONE =>	(* local *)
			  case IdentDict.find(TE, tycon)
			    of NONE => deps
			     | SOME index => index :: deps
		  end
	       | Absyn.TUPLEty(tyseq, _) => List.foldl depsOf deps tyseq
	       | Absyn.RELty(tyseq1, tyseq2, _) =>
		  List.foldl depsOf (List.foldl depsOf deps tyseq1) tyseq2
		   | Absyn.NAMEDty(ident, ty, _) => depsOf (ty, deps)
      in
		depsOf
      end

    fun depsOfConBnd TE (Absyn.CONcb (_, _), deps) = deps
      | depsOfConBnd TE (Absyn.CTORcb(_,tyseq, _), deps) =
	  List.foldl (depsOfTy TE) deps tyseq

    fun analyseType(source, TE, depArr, i, tycon, ty) =
      let val deps = depsOfTy TE (ty,[])
	  (* SILENT CHANGE: Some type alias forms now become illegal.
	   * Consider: "type int = int * int". This worked under the
	   * sequential model, being the same as "type int = RML.int * RML.int",
	   * but now it is illegal since every use of an unqualified
	   * type constructor in a module must refer to the same binding.
	   * Since the type checker proper still uses the sequential model,
	   * we have to make additional checks here.
	   * Datatype bindings are not affected, however.
	   *)
	  val _ = if List.exists (fn j => i = j) deps then
		    idError(source, "type alias depends on itself: ", tycon)
		  else ()
      in
		Array.update(depArr, i, deps)
      end

    fun analyseTyBnd source TE depArr (i,TYPBND(Absyn.TYPBIND(_,tycon,ty, _))) =
	  analyseType(source, TE, depArr, i, tycon, ty)
      | analyseTyBnd source TE depArr (i,DATBND(Absyn.DATBIND(_,_,conbnds, _))) =
	  Array.update(depArr, i, List.foldl (depsOfConBnd TE) [] conbnds)

    fun analyseTyBnd' source TE depArr (i,Absyn.TYPBIND(_,tycon,ty, _)) =
      analyseType(source, TE, depArr, i, tycon, ty)

    (* REORDERING *)

    fun tyconOf(TYPBND(Absyn.TYPBIND(_,tycon,_, _))) = tycon
      | tyconOf(DATBND(Absyn.DATBIND(_,tycon,_, _))) = tycon

    fun addIndex source (i, tyb, TE) =
      let val tycon = tyconOf tyb
      in
	case IdentDict.find'(TE, tycon)
	  of NONE => IdentDict.insert(TE, tycon, i)
	   | SOME(tycon',_) =>
	      (sayIdError(source, "rebinding of type constructor ", tycon);
	       idError(source, "this is the other binding of ", tycon'))
      end

    fun addIndex'(i, Absyn.TYPBIND(_,tycon,_, _), TE) = IdentDict.insert(TE, tycon, i)

    fun checkSingleton _ [] = bug "checkSingleton []"
      | checkSingleton _ [typbnd] = typbnd
      | checkSingleton source typbnds =
	  let fun sayTy(Absyn.TYPBIND(_,tycon,_, _)) = sayIdError(source, "", tycon)
	  in
	    sayMsg "\nError: recursive set of type aliases:\n";
	    List.app sayTy typbnds;
	    raise ReorderTypeDeclarationsError
	  end

    fun mkWithBind(source, typbnds) =
      let val typVec = Vector.fromList typbnds
	  val TE = Vector.foldli addIndex' IdentDict.empty (typVec) (* , 0, NONE) *)
	  val depArr = Array.array(Vector.length typVec, []:int list)
	  val _ = Vector.appi (analyseTyBnd' source TE depArr) (typVec) (* , 0,NONE) *)
	  val depVec = Vector.tabulate(Vector.length typVec, fn i => (Vector.sub(typVec,i), Array.sub(depArr,i)))
	  val components = ReorderSCC.scc depVec
      in
		map (checkSingleton source) components
      end

    fun mkTyDec source tybLst =
      let fun split([], [], []) = bug "mkTyDec([],[],[])"
	    | split([], [], [typbnd]) = TYPDEC typbnd
	    | split([], datbnds, typbnds) = DATDEC(datbnds, mkWithBind(source, typbnds))
	    | split(tyb::tybLst, datbnds, typbnds) =
		(case tyb
		   of TYPBND typbnd => split(tybLst, datbnds, typbnd::typbnds)
		    | DATBND datbnd => split(tybLst, datbnd::datbnds, typbnds))
      in
	split(tybLst, [], [])
      end

    fun reorderTyBnds(source, tybLst) =
      let val tybVec = Vector.fromList tybLst
	  val TE = Vector.foldli (addIndex source) IdentDict.empty (tybVec) (* , 0, NONE) *)
	  val depArr = Array.array(Vector.length tybVec, []:int list)
	  val _ = Vector.appi (analyseTyBnd source TE depArr) (tybVec) (* , 0, NONE) *)
	  val depVec = Vector.tabulate(Vector.length tybVec, fn i => (Vector.sub(tybVec,i), Array.sub(depArr,i)))
	  val components = ReorderSCC.scc depVec
      in
		map (mkTyDec source) components
      end

  end (* functor ReorderTyFn *)
