(* reorder/reorder-val.sml *)

functor ReorderValFn(structure Util : UTIL
		     structure Absyn : ABSYN
		     structure ReorderSCC : REORDER_SCC
		     sharing type Absyn.IdentDict.Key.ord_key = Absyn.ident
		       ) : REORDER_VAL =
  struct

    structure Absyn = Absyn
    structure IdentDict = Absyn.IdentDict

    fun bug s = Util.bug("ReorderVal."^s)

    (* DATA TYPES *)

    datatype valbnd	= VALBND of Absyn.ident * Absyn.exp
			| RELBND of Absyn.relbind

    datatype valdec	= VALDEC of Absyn.ident * Absyn.exp
			| RELDEC of Absyn.relbind list

    (* ERROR MESSAGES *)

    fun sayMsg msg = TextIO.output(TextIO.stdErr, msg)

    fun sayIdError(source, msg, Absyn.IDENT(name, Absyn.INFO(left, right))) =
      Absyn.Source.sayMsg source ("Error: "^msg^name, left, right)

    exception ReorderValueDeclarationsError

    fun idError(source, msg, id) =
      (sayIdError(source, msg, id);
       raise ReorderValueDeclarationsError)

    (* DEPENDENCY ANALYSIS *)

    fun scanPat(pat, VE) =
      case pat
		of Absyn.WILDpat (_) => VE
		 | Absyn.LITpat (_,_) => VE
		 | Absyn.CONpat (_,_) => VE
		 | Absyn.STRUCTpat(_,pats,_,_) => List.foldl scanPat VE pats
		 | Absyn.BINDpat(var,pat,_) => scanPat(pat, IdentDict.insert(VE, var, ~1))
		 | Absyn.IDENTpat(var,_, _) =>
			(* Reordering is done before StatElab has had a chance to qualify
			 * this identifier occurrence as a constant or a variable binding.
			 * Either choice is safe, however.
			 *)
			IdentDict.insert(VE, var, ~1)
		 | Absyn.NAMEDpat(ident, pat, _) => scanPat(pat, VE)

    fun depsOfVar(VE, var, deps) =
      case IdentDict.find(VE, var)
	of NONE => deps
	 | SOME ~1 => deps	(* local variable *)
	 | SOME index => index :: deps

    fun depsOfLongid(VE, Absyn.LONGID(SOME _, _, _), deps) = deps
      | depsOfLongid(VE, Absyn.LONGID(NONE, var, _), deps) = depsOfVar(VE, var, deps)

    fun depsOfExp VE =
      let fun depsOf(exp, deps) =
	    case exp
	      of Absyn.LITexp (_, _) => deps
	       | Absyn.CONexp (_, _) => deps
	       | Absyn.VARexp (longid, _) => depsOfLongid(VE, longid, deps)
	       | Absyn.STRUCTexp(_, exps, _) => List.foldl depsOf deps exps
	       | Absyn.IDENTexp(longid, _, _) => depsOfLongid(VE, longid, deps)
      in
		depsOf
      end

    fun depsOfGoal(goal, (VE,deps)) =
      case goal of 
		Absyn.CALLgoal(longid, exps, pats, _, _) =>
	    (List.foldl scanPat VE pats,
	     List.foldl (depsOfExp VE) (depsOfLongid(VE,longid,deps)) exps)
	 | Absyn.EQUALgoal(var, exp, _) => (VE,depsOfVar(VE,var,depsOfExp VE (exp,deps)))
	 | Absyn.LETgoal(pat, exp, _) => (scanPat(pat,VE), depsOfExp VE (exp,deps))
	 | Absyn.NOTgoal(goal, _) => (VE, #2(depsOfGoal(goal, (VE,deps))))
	 | Absyn.ANDgoal(g1,g2, _) => depsOfGoal(g2, depsOfGoal(g1, (VE,deps)))
	 | Absyn.CONDgoal(g1, g2, g3, _) => depsOfGoal(g3, depsOfGoal(g2, depsOfGoal(g1, (VE,deps))))

    fun depsOfGoalOpt(NONE, VE, deps) = (VE,deps)
      | depsOfGoalOpt(SOME goal, VE, deps) = depsOfGoal(goal, (VE,deps))

    fun depsOfResult(VE, deps, result) =
      case result
	of Absyn.RETURN(exps, _) => List.foldl (depsOfExp VE) deps exps
	 | Absyn.FAIL (_) => deps

    fun depsOfClause(VE, clause, deps) =
      case clause
	of Absyn.CLAUSE1(goal_opt, _, pats, result, _, localVars, _) =>
	    let val VE = List.foldl scanPat VE pats
		val (VE,deps) = depsOfGoalOpt(goal_opt, VE, deps)
	    in
	      depsOfResult(VE, deps, result)
	    end
	 | Absyn.CLAUSE2(cl1, cl2, _) => depsOfClause(VE, cl2, depsOfClause(VE,cl1,deps))

    fun analyseValBnd source VE depArr (i,VALBND(var,exp)) =
	  let val deps = depsOfExp VE (exp,[])
	      (* SILENT CHANGE: Some variable declarations now become illegal.
	       * Consider: "val int_add = int_add". This worked under the
	       * sequential model, being the same as "val int_add = RML.int_add",
	       * but now it is illegal since every use of an unqualified
	       * value constructor in a module must refer to the same binding.
	       * Since the type checker proper still uses the sequential model,
	       * we have to make additional checks here.
	       * Relation bindings are not affected, however.
	       *)
	      val _ = if List.exists (fn j => i = j) deps then
					idError(source, "variable depends on itself: ", var)
				  else ()
	  in
	    Array.update(depArr, i, deps)
	  end
      | analyseValBnd source VE depArr (i,RELBND(Absyn.RELBIND(_,_,clause, _, _, _))) =
			Array.update(depArr, i, depsOfClause(VE,clause,[]))

    (* REORDERING *)

    fun varOf(VALBND(var,_)) = var
      | varOf(RELBND(Absyn.RELBIND(var, _, _, _, _, _))) = var

    fun addIndex source (i, valb, VE) =
      let val var = varOf valb
      in
	case IdentDict.find'(VE, var)
	  of NONE => IdentDict.insert(VE, var, i)
	   | SOME(var',_) =>
	      (sayIdError(source, "rebinding of variable ", var);
	       idError(source, "this is the other binding of ", var'))
      end

    fun mkValDec source valbLst =
      let fun split([], [], []) = bug "mkValDec: split([],[],[])"
	    | split([], [], [valbnd]) = VALDEC valbnd
	    | split([], [], valbnds) =
		let fun sayVar(var,_) = sayIdError(source, "", var)
		in
		  sayMsg "\nError: recursive set of variable bindings:\n";
		  List.app sayVar valbnds;
		  raise ReorderValueDeclarationsError
		end
	    | split([], relbnds, []) = RELDEC relbnds
	    | split([], relbnds, valbnds) =
		let fun sayRel(Absyn.RELBIND(var, _, _, _, _, _)) =
		      sayIdError(source, "relation ", var)
		    fun sayVar(var,_) = sayIdError(source, "variable ", var)
		in
		  sayMsg "\nError: circularity between relations and variables:\n";
		  List.app sayRel relbnds;
		  List.app sayVar valbnds;
		  raise ReorderValueDeclarationsError
		end
	    | split(valb::valbLst, relbnds, valbnds) =
		(case valb
		   of VALBND v => split(valbLst, relbnds, v::valbnds)
		    | RELBND r => split(valbLst, r::relbnds, valbnds))
      in
		split(valbLst, [], [])
      end

    fun reorderValBnds(source, valbLst) =
      let val valbVec = Vector.fromList valbLst
	  val VE = Vector.foldli (addIndex source) IdentDict.empty (valbVec) (* , 0, NONE) *)
	  val depArr = Array.array(Vector.length valbVec, []: int list)
	  val _ = Vector.appi (analyseValBnd source VE depArr) (valbVec) (* , 0, NONE) *)
	  val depVec = Vector.tabulate(Vector.length valbVec,
				       fn i => (Vector.sub(valbVec,i),
						Array.sub(depArr,i)))
	  val components = ReorderSCC.scc depVec
      in
		map (mkValDec source) components
      end

  end (* functor ReorderValFn *)
