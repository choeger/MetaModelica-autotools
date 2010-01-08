(* interp/interp.sml *)

functor InterpFn(structure Util : UTIL
		 structure InterpCore : INTERP_CORE 
	     sharing type InterpCore.Absyn.IdentDict.Key.ord_key = InterpCore.Absyn.ident
		 ) : INTERP =
  struct

    structure Absyn = InterpCore.Absyn
    structure StrDict = InterpCore.Absyn.IdentDict

    fun bug s = Util.bug("Interpreter: "^s)

    fun answer a = (print "answer: "; print a; print "\n")

    (* Compound Objects *)

    type venv = InterpCore.value StrDict.dict
    type menv = venv StrDict.dict

    datatype fcont
      = ORELSE of InterpCore.marker * Absyn.clause * InterpCore.value list * menv * venv * fcont * pcont
      | ORNOT of InterpCore.marker * gcont * venv * fcont
      | ORHALT
    and gcont
      = ANDTHEN of Absyn.goal * menv * gcont
      | ANDNOT of fcont
      | ANDRETURN of menv * Absyn.result * pcont
      | ANDHALT
    and pcont
      = RETMATCH of Absyn.pat list * venv * gcont * fcont

    (* Pattern Matching *)

    fun match(pat, v) =
      case pat
	of Absyn.WILDpat _ => SOME StrDict.empty
	 | Absyn.LITpat(lit1, _) =>
	    (case v
	       of InterpCore.LIT lit2 =>
		    if Absyn.litEqual(lit1, lit2) then SOME StrDict.empty else NONE
		| _ => bug "match: expected LIT")
	 | Absyn.CONpat(Absyn.LONGID(_, con1, _), _) =>
	    let val con1 = Absyn.identName con1
	    in
	      case v
		of InterpCore.STRUCT(con2, _) =>
		    if con1=con2 then SOME StrDict.empty else NONE
		 | _ => bug("match: constant "^con1^" expected STRUCT(_,_)")
	    end
	 | Absyn.STRUCTpat(SOME(Absyn.LONGID(_, con1, _)), pat_star, _, _) =>
	    let val con1 = Absyn.identName con1
	    in
	      case v
		of InterpCore.STRUCT(con2,v_star) =>
		    if con1=con2 then match_star(pat_star, v_star) else NONE
		 | _ => bug("match: constructor "^con1^" expected STRUCT(_,_)")
	    end
	 | Absyn.STRUCTpat(NONE, pat_star, _, _) =>
	    (case v
	       of InterpCore.TUPLE v_star => match_star(pat_star, v_star)
		| _ => bug "match: expected TUPLE")
	 | Absyn.BINDpat(var, pat, _) =>
	    (case match(pat, v)
	       of SOME VE	=> SOME(StrDict.insert(VE, var, v))
			| NONE		=> NONE)
	 | Absyn.IDENTpat(_, r, _) => match(!r, v)
	 | Absyn.NAMEDpat(id, pat, _) => 
	    (bug "named arguments in patterns not implemented yet in the Interpreter!")
	 
    and match_star(pat_star, v_star) =
      let fun loop([], [], VE) = SOME VE
	    | loop(pat::pat_star, v::v_star, VE) =
		(case match(pat, v)
		   of SOME VE'	=> loop(pat_star, v_star, StrDict.plus(VE,VE'))
		    | NONE	=> NONE)
	    | loop(_, _, _) = bug "match_star: arity mismatch"
      in
		loop(pat_star, v_star, StrDict.empty)
      end

    (* Long Variables *)

    fun lookupModid(ME, modid) =
      case StrDict.find(ME, modid)
		of SOME VE => VE
		 | NONE => bug("module "^Absyn.identName modid^" unbound")

    fun lookupVar(VE, var) =
      case StrDict.find(VE, var)
	of SOME value => value
	 | NONE => bug("variable "^Absyn.identName var^" unbound")

    fun lookupLongVar(longvar, ME, VE) =
      case longvar
	of Absyn.LONGID(NONE, var, _)	=> lookupVar(VE, var)
	 | Absyn.LONGID(SOME modid, var, _) => lookupVar(lookupModid(ME,modid), var)

    (* Expressions *)

    fun eval(exp, ME, VE) =
      case exp
	of Absyn.LITexp(lit, _) => InterpCore.LIT lit
	 | Absyn.CONexp(Absyn.LONGID(_, con, _), _) =>
	    InterpCore.STRUCT(Absyn.identName con, [])
	 | Absyn.VARexp(longvar, _) => lookupLongVar(longvar,ME,VE)
	 | Absyn.STRUCTexp(SOME(Absyn.LONGID(_, con, _)), exp_star, _) =>
	    let val v_star = eval_star(exp_star, ME, VE)
		val con = Absyn.identName con
	    in
	      InterpCore.STRUCT(con, v_star)
	    end
	 | Absyn.STRUCTexp(NONE, exp_star, _) =>
	    let val v_star = eval_star(exp_star, ME, VE)
	    in
	      InterpCore.TUPLE v_star
	    end
	 | Absyn.IDENTexp(_, r, _) => eval(!r, ME, VE)

    and eval_star(exp_star, ME, VE) =
      map (fn exp => eval(exp, ME, VE)) exp_star

    (* Recursive Values *)

    fun unfold_v(VE_rec, v) =
      case v
	of InterpCore.CLOSURE{clause, ME, VE,...} =>
	    InterpCore.CLOSURE{clause=clause, ME=ME, VE=VE, VE_rec=VE_rec}
	 | _ => v

    (* Recursive Value Environments *)

    fun unfold_VE VE =
      StrDict.fold((fn(var,v,VE') => StrDict.insert(VE', var, unfold_v(VE,v))),
		   StrDict.empty, VE)

    (* Failure Continuations *)

    fun fail(fc, s) =
      case fc
	of ORELSE(m,clause,v_star,ME,VE,fc,pc) =>
	    let val s' = InterpCore.restore(m, s)
	    in
	      invoke(clause, v_star, ME, VE, fc, pc, s')
	    end
	 | ORNOT(m,gc,VE,fc) =>
	    let val s' = InterpCore.restore(m, s)
	    in
	      proceed(gc, VE, fc, s')
	    end
	 | ORHALT => answer "No"

    (* Goal Continuations *)

    and proceed(gc, VE, fc, s) =
      case gc
	of ANDTHEN(goal,ME,gc) => exec(goal, ME, VE, fc, gc, s)
	 | ANDNOT(fc') => fail(fc', s)
	 | ANDRETURN(ME, Absyn.RETURN(exp_star, _), pc) =>
	    let val v_star = eval_star(exp_star, ME, VE)
	    in
	      return(pc, v_star, s)
	    end
	 | ANDRETURN(_, Absyn.FAIL _, _) => fail(fc, s)
	 | ANDHALT => answer "Yes"

    (* Procedure Continuations *)

    and return(RETMATCH(pat_star,VE,gc,fc), v_star, s) =
      case match_star(pat_star, v_star)
	of SOME VE' => proceed(gc, StrDict.plus(VE,VE'), fc, s)
	 | NONE => fail(fc, s)

    (* Procedure Calls *)

    and call(v, v_star, fc, pc, s) =
      case v
	of InterpCore.CLOSURE{clause,ME,VE,VE_rec} =>
	    let val VE'_rec = unfold_VE VE_rec
	    in
	      invoke(clause, v_star, ME, StrDict.plus(VE,VE'_rec), fc, pc, s)
	    end
	 | InterpCore.PRIM prim =>
	    (case InterpCore.APPLY(prim, v_star, s)
	       of InterpCore.SUCCESS(v'_star,s') => return(pc, v'_star, s')
		| InterpCore.FAILURE s' => fail(fc, s'))
	 | _ => bug "call: expected CLOSURE or PRIM"

    (* Goals *)

    and exec(goal, ME, VE, fc, gc, s) =
      case goal
	of Absyn.CALLgoal(longvar, exp_star, pat_star, _, _) =>
	    let val v = lookupLongVar(longvar, ME, VE)
		val v'_star = eval_star(exp_star, ME, VE)
		val pc = RETMATCH(pat_star, VE, gc, fc)
	    in
	      call(v, v'_star, fc, pc, s)
	    end
	 | Absyn.EQUALgoal(var, exp, _) =>
	    let val v = eval(exp, ME, VE)
	    in
	      case StrDict.find(VE, var)	(*XXX: implicit let crap*)
		of NONE => proceed(gc, StrDict.insert(VE,var,v), fc, s)
		 | SOME v' =>
		    if InterpCore.equal(v,v') then proceed(gc, VE, fc, s)
		    else fail(fc, s)
	    end
	 | Absyn.LETgoal(pat, exp, _) =>
	    let val v = eval(exp, ME, VE)
	    in
	      case match(pat, v)
		of SOME VE' => proceed(gc, StrDict.plus(VE,VE'), fc, s)
		 | NONE => fail(fc, s)
	    end
	 | Absyn.NOTgoal(goal, _) =>
	    let val m = InterpCore.marker s
		val fc' = ORNOT(m, gc, VE, fc)
		val gc' = ANDNOT(fc)
	    in
	      exec(goal, ME, VE, fc', gc', s)
	    end
	 | Absyn.ANDgoal(goal1, goal2, _) =>
	    let val gc' = ANDTHEN(goal2, ME, gc)
	    in
	      exec(goal1, ME, VE, fc, gc', s)
	    end

    and exec'(goal_opt, ME, VE, fc, gc, s) =
      case goal_opt
	of SOME goal => exec(goal, ME, VE, fc, gc, s)
	 | NONE => proceed(gc, VE, fc, s)

    (* Clauses *)

    and invoke(clause, v_star, ME, VE, fc, pc, s) =
      case clause
	of Absyn.CLAUSE1(goal_opt, _, pat_star, result, _, _, _) =>
	    (case match_star(pat_star, v_star)
	       of SOME VE' =>
		    let val gc = ANDRETURN(ME, result, pc)
		    in
		      exec'(goal_opt, ME, StrDict.plus(VE,VE'), fc, gc, s)
		    end
		| NONE => fail(fc, s))
	 | Absyn.CLAUSE2(clause1, clause2, _) =>
	    let val m = InterpCore.marker s
		val fc' = ORELSE(m, clause2, v_star, ME, VE, fc, pc)
	    in
	      invoke(clause1, v_star, ME, VE, fc', pc, s)
	    end

    (* Relation Bindings *)

    fun evalRel(ME, VE, relbinds) =
      let fun evRel(Absyn.RELBIND(var, _, clause, _, _, _), VE') =
	    StrDict.insert(VE', var,
			   InterpCore.CLOSURE{clause=clause, ME=ME,
					      VE=VE, VE_rec=StrDict.empty})
      in
		List.foldl evRel StrDict.empty relbinds
      end

    (* Declarations *)

    fun evalDec(ME, VE, decs) =
      let fun evDec(Absyn.VALdec(var, exp, _), VE) =
		let val v = eval(exp, ME, VE)
		in
		  StrDict.insert(VE, var, v)
		end
	    | evDec(Absyn.RELdec(relbind, _), VE) =
		let val VE' = evalRel(ME, VE, relbind)
		    val VE'' = unfold_VE VE'
		in
		  StrDict.plus(VE, VE'')
		end
	    | evDec(_, VE) = VE
      in
		List.foldl evDec VE decs
      end

    (* Module Sequences *)

    fun load(ME, modseq) =
      let fun loadMod(Absyn.MODULE(Absyn.INTERFACE({modid, ...}, _), dec, _), ME) =
	    let val VE = evalDec(ME, InterpCore.VE_init, dec)
	    in
	      StrDict.insert(ME, modid, VE)
	    end
      in
		List.foldl loadMod ME modseq
      end

    (* Program Arguments *)

    fun cnvargv scon_star =
      InterpCore.list2value 
		(fn scon => 
			InterpCore.LIT(
				Absyn.SCONlit(scon, Absyn.dummyInfo))) scon_star

    (* Programs *)

    fun run(modseq, scon_star) =
      let val ME = load(InterpCore.ME_init, modseq)
	  val VE = lookupModid(ME, Absyn.rmlIdent "Main")
	  val v = lookupVar(VE, Absyn.rmlIdent "main")
	  val v' = cnvargv scon_star
	  val fc = ORHALT
	  val pc = RETMATCH([], StrDict.empty, ANDHALT, fc)
      in
		call(v, [v'], fc, pc, InterpCore.s_init)
      end

  end (* functor InterpFn *)
