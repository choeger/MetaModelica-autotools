(* absyntofol/absyntofol.sml *)

functor AbsynToFOLFn(structure StrDict : STR_DICT
		     structure Util : UTIL
		     structure Absyn : ABSYN
		     structure FOL : FOL
		     structure Instrument : ABSYN_INSTRUMENTED
		     sharing type FOL.Source.source = Absyn.Source.source = Instrument.Absyn.Source.source
		       ) : ABSYNTOFOL =
  struct

    structure Instrument = Instrument
    structure Absyn = Instrument.Absyn
    structure FOL = FOL
    structure Source = Instrument.Absyn.Source
	
    fun cvInfo(Absyn.INFO(spos, epos)) = FOL.INFO(spos, epos)

    fun cnvId(Absyn.IDENT(name, info)) = FOL.IDENT(name, cvInfo(info)) 
    fun cnvModidOpt(NONE) = NONE
      | cnvModidOpt(SOME modid) = SOME(cnvId modid)
    fun cnvLongid(Absyn.LONGID(modidOpt, id, info)) =
      let val modidOpt = cnvModidOpt modidOpt
	  val id = cnvId id
      in
		FOL.LONGID(modidOpt, id, cvInfo(info))
      end
    fun cnvLongidOpt(NONE) = NONE
      | cnvLongidOpt(SOME longid) = SOME(cnvLongid longid)

    fun cnvLit(Absyn.CCONlit(c, _)) = FOL.ICON(Char.ord c)
      | cnvLit(Absyn.ICONlit(i, _)) = FOL.ICON i
      | cnvLit(Absyn.RCONlit(r, _)) = FOL.RCON r
      | cnvLit(Absyn.SCONlit(s, _)) = FOL.SCON s

    fun mkPatVar(x, info) = FOL.newvar(FOL.makeIdent(Instrument.getPatAsString x, cvInfo info))
      
    fun newvars lst = 
      let fun loop([], vars) = vars
	      |   loop(hd::tail, vars) = 
		      let val var = mkPatVar(hd, Instrument.getInfoFromPat hd)
		      in
				loop(tail, var::vars)
		      end
      in
		loop(lst, [])
      end      
	
    fun makeVRef(longid as Absyn.LONGID(modidOpt, id, _), VE) =
      case modidOpt
		of SOME _ => FOL.GVAR(cnvLongid longid)
		| NONE =>
			case StrDict.find(VE, Absyn.identName id)
			of SOME var =>  (FOL.BVAR var)
			| NONE => ( FOL.GVAR(cnvLongid longid))

    fun cnvExp(Absyn.LITexp(lit, _), _) = FOL.LITexp(cnvLit lit)
      | cnvExp(Absyn.CONexp(longcon, _), _) = FOL.CONexp(cnvLongid longcon)
      | cnvExp(Absyn.VARexp(longvar, _), VE) = FOL.VARexp(makeVRef(longvar, VE))
      | cnvExp(Absyn.STRUCTexp(longconOpt, exps, _), VE) =
		FOL.STRUCTexp(cnvLongidOpt longconOpt, cnvExps(exps, VE))
      | cnvExp(Absyn.IDENTexp(_, r, _), VE) = cnvExp(!r, VE)

    and cnvExps(exps, VE) = map (fn exp => cnvExp(exp, VE)) exps
    
    fun cnvPat(pat, VE) =
      let val var = mkPatVar(pat, Instrument.getInfoFromPat pat)
		  val (pat', VE', varNEW) = cnvPat'(pat, var, VE)
      in
		(FOL.PAT(varNEW, pat'), VE')
      end

    and cnvPat'(Absyn.WILDpat info, x, VE) = (FOL.WILDpat, VE, x)
      | cnvPat'(Absyn.LITpat(lit, info), x, VE) = (FOL.LITpat(cnvLit lit), VE, x)
      | cnvPat'(Absyn.CONpat(longcon, info), x, VE) = (FOL.CONpat(cnvLongid longcon), VE, x)
      | cnvPat'(Absyn.STRUCTpat(longconOpt, pats, ref(pats_positional), info), x, VE) =
	  let val (pats', VE') = (* fixed the named arguments to positional *)
			if List.length pats_positional = 0 
			then cnvPats(pats, VE) 
			else cnvPats(pats_positional, VE)
	      val longconOpt = cnvLongidOpt longconOpt
	  in
	    (FOL.STRUCTpat(longconOpt, pats'), VE', x)
	  end
      | cnvPat'(Absyn.BINDpat(id, pat, _), var,  VE) =
		let val FOL.VAR(x, y) = var
			val var1 = FOL.VAR(x, cnvId id) 
		in
			cnvPat'(pat, var1, StrDict.insert(VE, Absyn.identName id, var1)) 
		end
      | cnvPat'(x as Absyn.IDENTpat(id, r, _), var, VE) = 
		let val FOL.VAR(x, y) = var
			val var1 = FOL.VAR(x, cnvId id) 
		in
			cnvPat'(!r, var1, VE)
		end
      | cnvPat'(Absyn.NAMEDpat(id, pat, _), var, VE) = 
		let val FOL.VAR(x, y) = var
			val var1 = FOL.VAR(x, FOL.makeIdent(Instrument.getPatAsString pat, cvInfo(Instrument.getInfoFromPat pat)))
		in
			cnvPat'(pat, var1, VE)
		end

    and cnvPats(pats, VE) =
      let fun loop([], pats', VE) = (List.rev pats', VE)
	    | loop(pat::pats, pats', VE) =
		let val (pat', VE) = cnvPat(pat, VE)
		in
		  loop(pats, pat'::pats', VE)
		end
      in
		loop(pats, [], VE)
      end
	
    fun cnvGoal(Absyn.CALLgoal(longvar, exps, pats, ref(pats_positional), info), VE) =
	  let val vref = makeVRef(longvar, VE)
	      val exps' = cnvExps(exps, VE)
	      val (pats', VE) = (* fixed the named arguments to positional *)
			if List.length pats_positional = 0 
			then cnvPats(pats, VE)
			else cnvPats(pats_positional, VE)
	      and vars = newvars(
				if List.length pats_positional = 0 
				then pats 
				else pats_positional)
	  in
	    (FOL.AND(
			FOL.CALL(vref, exps', vars, cvInfo info),
		    FOL.MATCH(ListPair.zip(vars, pats'), cvInfo info), cvInfo info),
	     VE)
	  end
      | cnvGoal(Absyn.EQUALgoal(id, exp, info), VE) =
	  let val exp = cnvExp(exp, VE)
	      val id = cnvId id
	  in
	    case StrDict.find(VE, FOL.identName id)	(*XXX: implicit let crap*)
	      of SOME var => (FOL.EQUAL(var, exp, cvInfo info), VE)
	       | NONE =>
			  let val var = FOL.newvar(id)
			  in
				(FOL.BIND(var, exp, cvInfo info), StrDict.insert(VE, FOL.identName id, var))
			  end
	  end
      | cnvGoal(Absyn.LETgoal(pat, exp, info), VE) =
	  let val exp' = cnvExp(exp, VE)
	      val (pat', VE) = cnvPat(pat, VE)
	      val var = FOL.newvar(FOL.dummyIdent)
	  in
	    (FOL.AND(FOL.BIND(var, exp', cvInfo info), FOL.MATCH([(var, pat')], cvInfo info), cvInfo info), VE)
	  end
      | cnvGoal(Absyn.NOTgoal(g, info), VE) =
	  let val (conj, VE) = cnvGoal(g, VE)
	  in
	    (FOL.NOT(conj, cvInfo info), VE)
	  end
	  (*
      | cnvGoal(Absyn.ANDgoal(Absyn.CONDgoal(g1, g2, g3, info1), g4, info2), VE) =
		cnvGoal(Absyn.CONDgoal(g1, Absyn.ANDgoal(g2, g4, info1), Absyn.ANDgoal(g3, g4, info1), info2), VE)
	  *)
      | cnvGoal(Absyn.ANDgoal(g1, g2, info), VE) =
	  let val (conj1, VE) = cnvGoal(g1, VE)
	      val (conj2, VE) = cnvGoal(g2, VE)
	  in
	    (FOL.AND(conj1, conj2, cvInfo info), VE)
	  end
      | cnvGoal(Absyn.CONDgoal(g1, g2, g3, info), VE) =
	  let val (conj1, VE) = cnvGoal(g1, VE)
	      val (conj2, VE) = cnvGoal(g2, VE)
	      val (conj3, VE) = cnvGoal(g3, VE)
	  in
	    (FOL.IF(conj1, conj2, conj3, cvInfo info), VE)
	  end 

    fun cnvClause vars =
      let fun mkdisj(Absyn.CLAUSE1(goalOpt, _, pats, result, ref(pats_positional), _, infoCLAUSE)) =
		let val (pats', VE) = (* fixed the named arguments to positional *)
			if List.length pats_positional = 0 
			then cnvPats(pats, StrDict.empty)
			else cnvPats(pats_positional, StrDict.empty)
		    fun return VE =
		      case result
				of Absyn.RETURN(exps, info) => FOL.RETURN(cnvExps(exps, VE), cvInfo info)
				 | Absyn.FAIL info => FOL.FAIL(cvInfo info)
			val body = case goalOpt
				 of SOME goal =>
					  let val (conj, VE) = cnvGoal(goal, VE)
					  in
						FOL.ANDTHEN(conj, return VE, cvInfo infoCLAUSE)
					  end
				  | NONE => return VE
		in
		  FOL.ANDTHEN(FOL.MATCH(ListPair.zip(vars, pats'), cvInfo infoCLAUSE), body, cvInfo infoCLAUSE)
		end
	    | mkdisj(Absyn.CLAUSE2(cl1, cl2, info)) = FOL.ORELSE(mkdisj cl1, mkdisj cl2, cvInfo info)
      in
		mkdisj
      end

	(*
    fun clauseArity(Absyn.CLAUSE1(_, _, pats, _, ref(pats_positional), _, _)) = 
			if List.length pats_positional = 0 (* fixed the named arguments to positional *)
			then pats
			else pats_positional
      | clauseArity(Absyn.CLAUSE2(cl1, _, _)) = clauseArity cl1
    *)

    fun cnvRelBind (Absyn.RELBIND(var, _, cl, localVars, _, info)) =
      let (*val vars = newvars(clauseArity cl)*)
		   fun loop([]) = []
		   |   loop((id,_,_,attr)::tail) = 
			   let val Absyn.ATTRIBUTES{input,...} = attr  
			   in
					if (!input) 
					then FOL.newvar(cnvId id)::loop(tail) 
					else loop(tail)
			   end
		   val vars = loop(localVars) 
      in
		FOL.REL(cnvId var, vars, cnvClause vars cl, cvInfo info)
      end

    fun cnvConBind(Absyn.CONcb(con, _)) = FOL.CONcd(cnvId con)
      | cnvConBind(Absyn.CTORcb(con, tys, _)) = FOL.CTORcd(cnvId con, List.length tys)
    fun cnvDatBind(Absyn.DATBIND(_, tycon, conbinds, _)) =
      FOL.DATDESC(cnvId tycon, map cnvConBind conbinds)

    fun cnvSpecs specs =
      let fun cnvSpec(spec, specs) =
	    case spec
	      of Absyn.WITHspec(_, ri, _) => FOL.WITHspec(cnvInterface(!ri))::specs
	       | Absyn.ABSTYPEspec _ => specs
	       | Absyn.TYPEspec _ => specs
	       | Absyn.DATAspec(datbinds, _, _) =>
			 FOL.DATAspec(map cnvDatBind datbinds)::specs
	       | Absyn.VALspec(var, _, _) => FOL.VALspec(cnvId var)::specs
	       | Absyn.RELspec(var, _, _) => FOL.RELspec(cnvId var)::specs
      in
		List.rev(List.foldl cnvSpec [] specs)
      end

    and cnvInterface(Absyn.INTERFACE({modid,specs,...}, _)) =
      FOL.INTERFACE(cnvId modid, cnvSpecs specs)

    fun cnvDecs decs =
      let fun cnvDec(dec, decs) =
	    case dec
	      of Absyn.WITHdec(_, ri, _) => FOL.WITHdec(cnvInterface(!ri))::decs
	       | Absyn.TYPEdec _ => decs
	       | Absyn.DATAdec(datbinds, _, _) =>
			 FOL.DATAdec(map cnvDatBind datbinds)::decs
	       | Absyn.VALdec(var, exp, _) =>
			 FOL.VALdec(cnvId var, cnvExp(exp, StrDict.empty))::decs
	       | Absyn.RELdec(relbinds, _) =>
			 FOL.RELdec(map cnvRelBind relbinds)::decs
      in
				List.rev(List.foldl cnvDec [] decs)
      end

    fun translate(Absyn.MODULE(interface as Absyn.INTERFACE({source,...}, _), decs, _)) = 
    (
    	(* print ("AbsynToFOL:"^Source.getFileName(source)^"\n"); *)
		FOL.MODULE(cnvInterface interface, cnvDecs decs, source)
	)

  end (* functor AbsynToFOLFn *)
