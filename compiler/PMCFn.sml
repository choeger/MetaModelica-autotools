(* fol2cps/pmc.sml *)

functor PMCFn(
	structure Util : UTIL
	structure CPS : CPS
	structure Control : CONTROL) : PMC =
  struct

    fun bug s = Util.bug("PMC."^s)
    fun sayErr s = TextIO.output(TextIO.stdErr, s)

    (*
     * Interface types
     *)
    structure CPS = CPS

    datatype pat
      = LITpat of CPS.constant
      | CONpat of {ncons: int, con: int, name: CPS.longid}
      | STRUCTpat of {ncons: int, con: int, pats: pat list, name:CPS.longid}
      | WILDpat
      | BINDpat of CPS.var * pat


   fun pmc(roots, mrules, fail, warnNonExhaustive, printDFAStatistics, lid, info) =
   let 
    (*
     * Utilities
     *)
    fun list_delnth(_::xs, 0) = xs
      | list_delnth(x::xs, n) = x :: list_delnth(xs, n-1)
      | list_delnth(_, _) = bug "list_delnth"

    fun sublist indices xs =
      (* Select elements from `xs' based on `indices'.
       * E.g., sublist [0,2] [a,b,c] returns [a,c].
       *)
      let fun subl(_, [], _) = []
	    | subl([], _, _) = []
	    | subl(x::xs, indices as (i::is), j) =
		if i=j then x :: subl(xs, is, j+1)
		else subl(xs, indices, j+1)
      in
	subl(xs, indices, 0)
      end

    (* turn [[1,2,3],[4,5,6],[7,8,9]] into [[1,4,7],[2,5,8],[3,6,9]] *)
    fun transp([], _) = []
      | transp(x::row0, rows) = (x :: map hd rows) :: transp(row0, map tl rows)
    fun transpose [] = []
      | transpose(row0::rows) = transp(row0, rows) handle _ => bug "transpose"

    fun sort lt xs =
      let fun insert(x, []) = [x]
	    | insert(x, ys as (y::ys')) =
		if lt(x,y) then x::ys else y::insert(x,ys')
      in
		List.foldl insert [] xs
      end

    fun push(x, rxs) = rxs := x :: !rxs

    fun setMember eq x ys =
      List.exists (fn y => eq(x,y)) ys

    fun setDiff eq xs ys =	(* return zs = xs\ys *)
      let fun filter(x, zs) = if setMember eq x ys then zs else x::zs
      in
		List.foldl filter [] xs
      end

    fun setCons eq x xs = if setMember eq x xs then xs else x::xs

    fun setUnion eq xs ys =
      List.foldl (fn(x,ys) => setCons eq x ys) ys xs

    (*
     * Occurrences
     * Due to splits and joins in the DAG, we cannot decide which CPS.Var
     * to use for an occurrence until we generate code for its binding.
     *)
    abstype occ
      = OCC of {var: CPS.var ref,	(* for equality and naming *)
		subs: (int * occ) list ref}	(* for identity of sub-occs *)
    with
      fun occEq(OCC{var=var1,...}, OCC{var=var2,...}) = var1=var2
      fun occVar(OCC{var,...}) = var
      fun varOcc var = OCC{var=ref var, subs=ref[]}
      local
		val dummyVar = CPS.newVar(CPS.dummyLongIdent)
      in
		fun occSub(OCC{subs,var,...}, i) =
		  let fun look([]) =
				let val occ = OCC{var=ref (!var) (* dummyVar *), subs=ref[]}
				val _ = push((i,occ), subs)
				in
				  occ
				end
			| look((i',occ')::subs) = if i=i' then occ' else look subs
		  in
			look(!subs)
		  end
      end
    end

    val occMember = setMember occEq
    val occDiff = setDiff occEq
    val occCons = setCons occEq
    val occUnion = setUnion occEq

    (*
     * Pattern-Matching Automata
     *)
    datatype state		(* common attributes *)
      = STATE of {label: CPS.label ref,	(* for comparisons and name generation *)
				  fvs: occ list,	(* free occurrences *)
				  uses: int ref,	(* useless, inlined, or shared? *)
				  defn: state' }
    and state'
      = FINALq of (CPS.var * occ) list * (CPS.trivexp -> CPS.exp)
      | FAILq of CPS.trivexp
      | ORELSEq of state * state
      | FETCHq of occ * (occ * int) list * state
      | CASEq of occ * (CPS.constant * state) list * state option

    val allStates = ref([]: state list)
    local
      val dummyBody = CPS.mkAppFCe{fc=CPS.mkQUOTEte(CPS.CONSTlit(CPS.STRINGcon "PMC BUG!")), name=lid, pos=info}
    in
      fun newState(defn, fvs) =
			let val q = STATE{label=ref(CPS.newLab([], dummyBody, lid, info)),
					  fvs=fvs, uses=ref 0, defn=defn}
				val _ = push(q, allStates)
			in
			  q
			end
    end

    fun dfaStatistics() =
      let fun defaultNumArcs(NONE) = 0
	    | defaultNumArcs(SOME _) = 1
	  fun stateNumArcs(FINALq _) = 0
	    | stateNumArcs(FAILq _) = 0
	    | stateNumArcs(ORELSEq _) = 2
	    | stateNumArcs(FETCHq _) = 1
	    | stateNumArcs(CASEq(_,arcs,default)) =
		length arcs + defaultNumArcs default
	  fun count([], nstates, narcs, nshared) =
	       (print ("(*dfa statistics for: "^(CPS.longIdentName lid)^" -> nstates="); print(Int.toString nstates);
		print " (nshared="; print(Int.toString nshared);
		print "), narcs="; print(Int.toString narcs); print "*)\n")
	    | count((STATE{uses,defn,...})::states, nstates, narcs, nshared) =
		count(states, nstates + 1, narcs + stateNumArcs defn,
		      if !uses > 1 then nshared+1 else nshared)
      in
		count(!allStates, 0, 0, 0)
      end

    fun stateIncRef(STATE{uses,...}) = uses := !uses + 1
    fun stateFvs(STATE{fvs,...}) = fvs
    fun stateEq(STATE{label=lab1,...},STATE{label=lab2,...}) = 
    	if !Control.doDebug 
    	then false (* do not share if in debug mode *)
    	else lab1=lab2 (*Ref.=*)
    fun stateLt(STATE{label=ref(CPS.LAB{tag=tag1,...}),...},
		STATE{label=ref(CPS.LAB{tag=tag2,...}),...}) = tag1 < tag2

    fun arcsFvs([], fvs) = fvs
      | arcsFvs((_,q)::arcs, fvs) = arcsFvs(arcs, occUnion (stateFvs q) fvs)
    fun arcIncRef(_,q) = stateIncRef q
    fun arcLt((_,q1), (_,q2)) = stateLt(q1, q2)
    fun arcEq((lit1,q1), (lit2,q2)) =
      stateEq(q1,q2) andalso CPS.constEqual(lit1,lit2)
    fun arcsEq([], []) = true
      | arcsEq(a1::arcs1, a2::arcs2) = arcEq(a1,a2) andalso arcsEq(arcs1,arcs2)
      | arcsEq(_, _) = false

    fun bndsBvs([], bvs) = bvs
      | bndsBvs((occ,_)::bnds, bvs) = bndsBvs(bnds, occCons occ bvs)
    fun bndEq((occ1,i1), (occ2,i2)) = i1=i2 andalso occEq(occ1,occ2)
    fun bndsEq([], []) = true
      | bndsEq(b1::bnds1, b2::bnds2) = bndEq(b1,b2) andalso bndsEq(bnds1,bnds2)
      | bndsEq(_, _) = false

    fun defaultIncRef(NONE) = ()
      | defaultIncRef(SOME q) = stateIncRef q
    fun defaultEq(NONE, NONE) = true
      | defaultEq(SOME q1, SOME q2) = stateEq(q1,q2)
      | defaultEq(_, _) = false
    fun defaultFvs(NONE) = []
      | defaultFvs(SOME q) = stateFvs q

    fun mkFAIL te = newState(FAILq te, [])

    fun mkFINAL(subst, mkexp) = newState(FINALq(subst, mkexp), map #2 subst)

    fun mkORELSE(q1, q2) =
      let fun search([]) =
					let val fvs = occUnion (stateFvs q1) (stateFvs q2)
					    val _ = stateIncRef q1
					    val _ = stateIncRef q2
					in
					  newState(ORELSEq(q1,q2), fvs)
					end
			    | search((q as STATE{defn=ORELSEq(q1', q2'),...})::qs) =
						if stateEq(q1,q1') andalso stateEq(q2,q2') 
						then q (* TODO! do not share! q *)
			 			else search qs
			    | search(_::qs) = search qs
      in
				search(!allStates)
      end

    fun mkFETCH(occ, bnds, body) =	(* XXX: should we sort `bnds'? *)
      let fun search([]) =
		let val bvs = bndsBvs(bnds,[])
		    val fvs = occCons occ (occDiff (stateFvs body) bvs)
		    val _ = stateIncRef body
		in
		  newState(FETCHq(occ,bnds,body), fvs)
		end
	    | search((q as STATE{defn=FETCHq(occ',bnds',body'),...})::qs) =
			if occEq(occ,occ')
			   andalso stateEq(body,body')
			   andalso bndsEq(bnds,bnds')
			  then q (* TODO! do not share! q *)
 			else search qs
	    | search(_::qs) = search qs
      in
		case bnds
		  of [] => body
		   | _ => search(!allStates)
      end

    fun mkCASE(occ, arcs, default) =
      let val arcs = sort arcLt arcs	(* ! *)
	  fun search([]) =
		let val fvs = occCons occ (arcsFvs(arcs, defaultFvs default))
		    val _ = List.app arcIncRef arcs
		    val _ = defaultIncRef default
		in
		  newState(CASEq(occ,arcs,default), fvs)
		end
	    | search((q as STATE{defn=CASEq(occ',arcs',default'),...})::qs) =
			if occEq(occ,occ') andalso defaultEq(default,default') andalso arcsEq(arcs,arcs')
			then q (* TODO! do not share! q *)
			else search qs
	    | search(_::qs) = search qs
      in
		case (default,arcs)
		  of (NONE,[(_,q)]) => q
		   | (SOME q,[]) => q		(* XXX: can this happen? *)
		   | _ => search(!allStates)
      end

    (*
     * Code Emission: convert the automaton to an expression
     *)
    fun occTE occ = CPS.mkVARte(!(occVar occ))

    fun mungeOcc occ =
      let val var = CPS.newVar(CPS.dummyLongIdent)
      in
		occVar occ := var;
		var
      end

    fun cnvArc t_fc (lit, q) = (lit, cnvState t_fc q)

    and cnvDefault t_fc (SOME q) = SOME(cnvState t_fc q)
      | cnvDefault _ NONE = NONE

    and cnvState t_fc (STATE{label,uses,fvs,defn,...}) = 
    if !Control.doDebug 
    then cnvDefn t_fc defn (* do not share! inline! *)
    else if !uses = 1 
         then cnvDefn t_fc defn
         else CPS.mkAppLABe(!label, t_fc :: map occTE fvs) 

    and cnvDefn t_fc (FAILq t_fc') = CPS.mkAppFCe{fc=t_fc', name=lid, pos=info}
      | cnvDefn t_fc (FINALq(subst,mkexp)) =
	  let fun mkBnds([], e) = e
		| mkBnds((var,occ)::subst, e) =
		    mkBnds(subst, CPS.mkLETe(var, occTE occ, e))
	  in
	    mkBnds(subst, mkexp t_fc)
	  end
      | cnvDefn t_fc (ORELSEq(q1,q2)) =
	  let val var = CPS.newVar(CPS.dummyLongIdent) (* lid *)
	  in
	    CPS.mkLETe(var, CPS.newLam(CPS.FClk, cnvState t_fc q2, lid, info), cnvState (CPS.mkVARte var) q1)
	  end
      | cnvDefn t_fc (FETCHq(occ,bnds,body)) =
	  let val node = occTE occ
	      fun mkFetches([]) = cnvState t_fc body
		| mkFetches((occ',off)::bnds) =
		    let val var = mungeOcc occ'
		    in
		      CPS.mkPRIMe(var, CPS.UNARYp(CPS.FETCH off,node), mkFetches bnds)
		    end
	  in
	    mkFetches bnds
	  end
      | cnvDefn t_fc (CASEq(occ,arcs,default)) =
		CPS.mkSWITCHe(occTE occ, map (cnvArc t_fc) arcs, cnvDefault t_fc default)

    fun findJoins qs =
      let fun loop([], joins) = joins
	    | loop((q as STATE{uses,...})::qs, joins) =
		  loop(qs, if !uses > 1 then q::joins else joins)
      in
		loop(qs, []) (* TODO! do not join! *)
      end

    fun cnvJoins qs =
      let fun mktrail occ = let val r = occVar occ in (r,!r) end
	  fun addBnd(STATE{label,fvs,defn,...}, labels) =
	    let val v_fc = CPS.newVar(CPS.dummyLongIdent) (* (lid) *)
		val trail = map mktrail fvs
		val vars = map mungeOcc fvs
		val lab = CPS.newLab(v_fc::vars, cnvDefn (CPS.mkVARte v_fc) defn, lid, info)
		val _ = List.app (op :=) trail
		val _ = label := lab
	    in
	      lab :: labels
	    end
      in
		List.foldl addBnd [] qs
      end

    fun cnvBnds([], exp) = exp
      | cnvBnds(lab::labels, exp) = cnvBnds(labels, CPS.mkLetLABe(lab, exp))

    fun dfaToExp t_fc (STATE{defn,...}) allStates =
      let val bnds = cnvJoins(findJoins allStates)
      in
		cnvBnds(bnds, cnvDefn t_fc defn)
      end

    (*
     * Pre-Processing
     *)
    datatype ppat	(* pat w/o variable bindings *)
      = LITpp of CPS.constant
      | CONpp of {ncons: int, con: int, name: CPS.longid}
      | STRUCTpp of {ncons: int, con: int, ppats: ppat list, name: CPS.longid}
      | WILDpp

    fun preProcessPats(roots, pats) =
      let fun ppPat(pat, occ, subst) =
	    case pat
	      of LITpat lit => (subst, LITpp lit)
	       | CONpat{ncons,con,name} => (subst, CONpp{ncons=ncons,con=con,name=name})
	       | STRUCTpat{ncons, con, pats, name} =>
		  let val (subst,ppats) = ppStruct(pats, occ, subst)
		  in
		    (subst, STRUCTpp{ncons=ncons,con=con,ppats=ppats, name=name})
		  end
	       | WILDpat => (subst, WILDpp)
	       | BINDpat(var,pat) => ppPat(pat, occ, (var,occ)::subst)
	  and ppStruct(pats, occ, subst) =
	    let fun ppi([], _, subst, ppats) = (subst, rev ppats)
		  | ppi(pat::pats, i, subst, ppats) =
		      let val (subst,ppat) = ppPat(pat, occSub(occ,i), subst)
		      in
				ppi(pats, i+1, subst, ppat::ppats)
		      end
	    in
	      ppi(pats, 1, subst, [])
	    end
	  fun loop([], [], subst, ppats) = (subst, rev ppats)
	    | loop(pat::pats, root::roots, subst, ppats) =
		let val (subst,ppat) = ppPat(pat, root, subst)
		in
		  loop(pats, roots, subst, ppat::ppats)
		end
	    | loop(_,_,_,_) = bug "preProcessPats.loop"
      in
		loop(pats, roots, [], [])
      end

    fun preProcessMatchRules(roots, mrules) =
      let val roots = map varOcc roots
	  fun preProcessMatchRule(pats, mkexp) =
	    let val (subst,ppats) = preProcessPats(roots, pats)
	    in
	      (ppats, mkFINAL(subst,mkexp))
	    end
	  val mrules = map preProcessMatchRule mrules
	  val finals = map #2 mrules
	  val rows = map #1 mrules
	  val preColumns = transpose rows
	  val columns = ListPair.zip(roots, preColumns)
      in
		(columns, finals)
      end

    (*
     * Column Representation Descriptors
     *)
    datatype rep	(* ppat w/o wildcard at top level *)
      = LITrep of CPS.constant
      | CONrep of {ncons: int, con: int, name: CPS.longid}
      | STRUCTrep of {ncons: int, con: int, ppats: ppat list, name: CPS.longid}
    type indices = int list
    datatype desc = DESC of rep * indices ref

    fun repEq(LITrep lit1, LITrep lit2) = CPS.constEqual(lit1,lit2)
      | repEq(CONrep{con=con1,...}, CONrep{con=con2,...}) = con1=con2
      | repEq(STRUCTrep{con=con1,...}, STRUCTrep{con=con2,...}) = con1=con2
      | repEq(_, _) = false

    fun describeColumn col =
      let fun pushi i (DESC(_,indices)) = push(i, indices)
	  fun build([], _) = ([], [])
	    | build(ppat::col, i) =
		let val (wilds,descs) = build(col, i+1)
		    fun addRep rep =
		      let fun update([]) = DESC(rep,ref(i::wilds))::descs
			    | update(DESC(rep',indices')::descs') =
				if repEq(rep,rep') then (push(i,indices'); descs)
				else update descs'
		      in
			(wilds, update descs)
		      end
		in
		  case ppat
		    of WILDpp => (List.app (pushi i) descs; (i::wilds, descs))
		     | LITpp lit => addRep(LITrep lit)
		     | CONpp{ncons,con,name} => addRep(CONrep{ncons=ncons,con=con,name=name})
		     | STRUCTpp{ncons,con,ppats,name} =>
			addRep(STRUCTrep{ncons=ncons,con=con,ppats=ppats,name=name})
		end
      in
	    build(col, 0)
      end

    fun descsAreExhaustive descs =
      let fun conCon(CONrep{con,...}) = con
	    | conCon(_) = bug "descsAreExhaustive.conCon"
	  fun structCon(STRUCTrep{con,...}) = con
	    | structCon(_) = bug "descsAreExhaustive.structCon"
	  fun consty(getCon, ncons, cons, descs) =
	    let fun loop([], cons) = ncons = length cons
		  | loop(DESC(rep,_)::descs, cons) =
		      loop(descs, setCons (op=) (getCon rep) cons)
	    in
	      loop(descs, cons)
	    end
      in
		case descs
		  of [] => true
		   | (DESC(rep,_)::descs) =>
			  case rep
			of LITrep _ => false	(* ok, a bit sloppy *)
			 | CONrep{ncons,con,...} => consty(conCon,ncons,[con],descs)
			 | STRUCTrep{ncons,con,...} => consty(structCon,ncons,[con],descs)
      end

    fun descsForStruct([]) = false
      | descsForStruct(DESC(STRUCTrep _,_)::_) = true
      | descsForStruct(_::_) = false

    (* Given is the occurrence of a column and a selection of its patterns.
     * All patterns should have the same top-level constructor, or be wildcards.
     * Lift the sub-patterns to make new columns; wildcards are expanded
     * by the arity of the column's constructor.
     * The new columns are tagged with their occurrences.
     *)
    fun newCols(occ, ppats) =
      let fun attachOccurrences([], _, columns) = rev columns
	    | attachOccurrences(ppats::preCols, i, columns) =
		attachOccurrences(preCols, i+1, (occSub(occ,i),ppats)::columns)
	  fun liftNew wilds =
	    let fun lift(WILDpp) = wilds
		  | lift(STRUCTpp{ppats,...}) = ppats
		  | lift(_) = bug "newCols.liftNew.lift"
	    in
	      attachOccurrences(transpose(map lift ppats), 1, [])
	    end
	  fun checkNew(STRUCTpp{ppats,...}::_) =
		liftNew(map (fn _ => WILDpp) ppats)
	    | checkNew(WILDpp::ppats) = checkNew ppats
	    | checkNew _ = []	(* no new columns *)
      in
		checkNew ppats
      end

    fun fetchStructElts(occ, ppats, q) =
      let val fvs = stateFvs q
	  fun loop([], _, bnds) = mkFETCH(occ, bnds, q)
	    | loop(_::ppats, i, bnds) =
		let val occi = occSub(occ,i)
		in
		  loop(ppats, i+1, if occMember occi fvs then (occi,i)::bnds else bnds)
		end
      in
		loop(ppats, 1, [])
      end

    fun fetchStructTag(occ, occ0, q) =
      if occMember occ0 (stateFvs q) 
      then mkFETCH(occ, [(occ0,0)], q)
      else q

    (*
     * Algorithm MATCH
     *)
    fun findColumn cols =
      let fun look([], _) = ~1		(* no columns to inspect *)
	    | look((_,[])::_, _) = ~1	(* ditto *)
	    | look((_,WILDpp::_)::cols, i) = look(cols, i+1)
	    | look((_,_::_)::_, i) = i	(* top-most pattern isn't a wildcard *)
      in
		look(cols, 0)
      end

    fun subCol indices (occ,ppats) = (occ, sublist indices ppats)
    fun tlCol(occ, ppats) = (occ, tl ppats)

    fun match(columns, finals, qfail) =
      case findColumn columns
		of ~1	=>
			(case finals
			   of []			=> qfail
			| [final]			=> final
			| (final::finals)	=>
				mkORELSE(final, match(map tlCol columns, finals, qfail)))
		 | k	=>
			mixrule(List.nth(columns,k), list_delnth(columns,k), finals, qfail)

    and mixrule((occ,ppats), cols, finals, qfail) =
      let fun mkCont indices =
	    match(newCols(occ,sublist indices ppats) @ map (subCol indices) cols,
		  sublist indices finals,
		  qfail)
	  val (wilds,descs) = describeColumn ppats
	  val default = 
			if descsAreExhaustive descs 
			then NONE
			else SOME(case wilds of [] => qfail | _ => mkCont wilds)
	  fun mkArc(DESC(rep, indices)) =
	    let val q = mkCont(!indices)
	    in
	      case rep
			of LITrep lit => (lit, q)
			 | CONrep{con,...} => (CPS.INTcon con, q)
			 | STRUCTrep{con,ppats,...} =>
				(CPS.HDRcon{len=length ppats, con=con},
				 fetchStructElts(occ, ppats, q))
	    end
	  val arcs = map mkArc descs
      in
		if descsForStruct descs 
		then
		  let val occ0 = occSub(occ,0)
		  in
			fetchStructTag(occ, occ0, mkCASE(occ0, arcs, default))
		  end
		else mkCASE(occ, arcs, default)
      end

    (*
     * Entry point
     *)
    fun checkIrredundancy qs =
      if List.all (fn(STATE{uses,...}) => !uses <> 0) qs then ()
      else sayErr "warning: match contains redundant cases\n"

    fun checkExhaustiveness(warn, STATE{uses,...}) =
      if !uses = 0 then ()
      else (if warn then sayErr "warning: match not exhaustive\n" else (); uses := 1)

  in
      let val _ = allStates := []
	  val qfail = mkFAIL fail
	  val (columns,finals) = preProcessMatchRules(roots, mrules)
	  val q0 = match(columns, finals, qfail)
      in
		stateIncRef q0;
		if (printDFAStatistics) then dfaStatistics() else ();
		checkIrredundancy finals;
		checkExhaustiveness(warnNonExhaustive, qfail);
		dfaToExp fail q0 (!allStates)
      end
  end

  end (* functor PMCFn *)
