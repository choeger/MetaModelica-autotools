
(* InstrumentFn.sml *)

functor InstrumentFn(structure Absyn : ABSYN
					 structure Util : UTIL
					 structure Control: CONTROL) : ABSYN_INSTRUMENTED =
  struct

    structure Absyn = Absyn
    
    fun bug s = Util.bug("Instrument."^s)
    
    (* XXX TODO: fix so that it takes the atomic expressions out of expresions!!! *)


    fun prStr(s) = (s)
	fun print_sequence(b4, between, after, foo_star, print_foo) =
	let fun loop([]) = ("")
		| loop(foo::foo_star) = (between^print_foo(foo)^(loop foo_star))
	in
		b4^
		(case foo_star
			of []		=> ("")
			| (foo::foo_star)	=> (print_foo(foo)^(loop foo_star)))
		^after
	end

    fun print_parens_comma(foo_star, print_foo) =
      print_sequence("(", ",", ")", foo_star, print_foo)
	
	
	fun print_list_sequence_exp(b4, between, after, foo_star, print_foo) =
	let fun loop([]) = ("")
		| loop(Absyn.CONexp(Absyn.LONGID(SOME(Absyn.IDENT("RML",_)),Absyn.IDENT("nil",_),_),_)::nil)
			= (after)
		| loop(Absyn.STRUCTexp(
				SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)), 
				                  Absyn.IDENT("cons", _), _)), foo::foo_star, _)::nil)
			= (between^print_foo(foo)^(loop foo_star))
		| loop(foo::foo_star) = (between^print_foo(foo)^(loop foo_star))
	in
		b4^
		(case foo_star
			of []		=> ("")
			| (foo::foo_star)	=> (print_foo(foo)^(loop foo_star)))
	end	
	
    fun print_list_comma_exp(foo_star, print_foo) =
      print_list_sequence_exp("[", ",", "]", foo_star, print_foo)

    (*
    RML.debug("main.rml",34,9,34,24,"apply","let:[[j,m]=lli") => () &
    let RML.cons(RML.cons(j,m),RML.nil) = lli &
    let [j::m] = lli &
    *)

	fun print_list_sequence_pat(b4, between, after, foo_star, print_foo) =
	let fun loop([]) = ("")
		| loop(Absyn.CONpat(Absyn.LONGID(SOME(Absyn.IDENT("RML",_)),Absyn.IDENT("nil",_),_),_)::nil)
			= (Control.selectCompilerMessage("nil","{}"))
		| loop(Absyn.STRUCTpat(
				SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)),
				     Absyn.IDENT("cons", _), _)), foo::foo_star, _, _)::nil)
			= (case foo_star of
			    (Absyn.STRUCTpat(
				 SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)),Absyn.IDENT("cons", _), _)),_,_,_)::nil) 
				 => (between^print_foo(foo)^(loop foo_star))
				 | (_) => (print_foo(foo)^"::"^(loop foo_star)))									
		| loop(foo::foo_star) = (between^print_foo(foo)^(loop foo_star))
	in
		b4^
		(case foo_star
			of []		=> ("")
			| (foo::foo_star)	=> (print_foo(foo)^(loop foo_star)))
		^after
	end	
	
    fun print_list_comma_pat(foo_star, print_foo) =
      print_list_sequence_pat("[", ",", "]", foo_star, print_foo)

    fun print_list([], _) = ("")
      | print_list(foo_star, print_foo) =
	  print_parens_comma(foo_star, print_foo)

    fun print_ident(id) = Absyn.identName id

    fun print_longid(Absyn.LONGID(modname, ident, _)) =
      (case modname
	  of SOME ident'=>
		 if Absyn.identName ident' = "RML"
		 then print_ident(ident)
		 else (print_ident(ident') ^ "." ^ print_ident(ident))
	   | NONE		=> (print_ident(ident)))

    (*
    fun print_longid_info(Absyn.LONGID(modname, ident, _)) = 
		let val Absyn.INFO(file,_,_,
						   Absyn.LOC(sline,scolumn,eline,ecolumn)) = 
						   Absyn.identCtxInfo ident
		in
		(case modname
			of SOME ident'=> (print_ident(ident') ^ "." ^ print_ident(ident))
			| NONE		=> (print_ident(ident))^("|"^file^":"^
				(Int.toString sline)^"."^
				(Int.toString scolumn)^"."^
				(Int.toString eline)^"."^
				(Int.toString ecolumn))
		)
	  end
    *)
	   
    fun print_lit(lit) = Absyn.litString lit

    fun print_scon(s) = MakeString.scvt s
    
    fun print_ctor_opt(NONE) = ("")
      | print_ctor_opt(SOME longctor) = print_longid(longctor)
    
	val modidRML = SOME(Absyn.rmlIdent "RML")
	val debug = Absyn.LONGID(modidRML, Absyn.rmlIdent "debug", Absyn.dummyInfo)
	val id_cons = Absyn.LONGID(modidRML, Absyn.rmlIdent "cons", Absyn.dummyInfo)
	val ctor_cons = SOME id_cons
	val dummyDebugVar = Absyn.LITexp(Absyn.SCONlit("-", Absyn.dummyInfo), Absyn.dummyInfo)
	val dummyMatch = Absyn.LITexp(Absyn.SCONlit("<- match", Absyn.dummyInfo), 
								  Absyn.dummyInfo)

    fun getPatAsString(Absyn.WILDpat _) = ("_")
      | getPatAsString(Absyn.LITpat(lit, _)) = print_lit(lit)
      | getPatAsString(Absyn.CONpat(longcon, _)) = print_longid(longcon)
      | getPatAsString(Absyn.STRUCTpat(ctor, pat_star, _, _)) =
		(print_ctor_opt(ctor)^print_parens_comma(pat_star, getPatAsString))
      | getPatAsString(Absyn.BINDpat(var, pat, _)) =
		(print_ident(var)^" as "^getPatAsString(pat))
      | getPatAsString(Absyn.IDENTpat(id, _, _)) = print_ident(id)
      | getPatAsString(Absyn.NAMEDpat(id, pat, _)) = print_ident(id)^" = "^getPatAsString(pat)

    fun getInfoFromPat(Absyn.WILDpat(info)) = info
      | getInfoFromPat(Absyn.LITpat(lit, info)) = info
      | getInfoFromPat(Absyn.CONpat(longcon, info)) = info
      | getInfoFromPat(Absyn.STRUCTpat(ctor, pat_star, _, info)) = info
      | getInfoFromPat(Absyn.BINDpat(var, pat, info)) = info
      | getInfoFromPat(Absyn.IDENTpat(id, _, info)) = info
      | getInfoFromPat(Absyn.NAMEDpat(id, pat, info)) = info
           
    fun getInfoFromExp(Absyn.LITexp(lit, info)) = info
      | getInfoFromExp(Absyn.CONexp(longcon, info)) = info
      | getInfoFromExp(Absyn.STRUCTexp(ctor, Exp_star, info)) = info
      | getInfoFromExp(Absyn.IDENTexp(id, _, info)) = info
      | getInfoFromExp(Absyn.VARexp(longId, info)) = info

    fun getExpAsString(Absyn.LITexp(lit, _)) = (Absyn.litString lit)
      | getExpAsString(Absyn.CONexp(longid, _)) = (print_longid(longid))
      | getExpAsString(Absyn.VARexp(longid, _)) = (print_longid(*_info*)(longid))
      | getExpAsString(Absyn.STRUCTexp(
				SOME(Absyn.LONGID(SOME(Absyn.IDENT("RML", _)), Absyn.IDENT("cons", _), _)), exp_star, _)) =
						(print_list_comma_exp(exp_star, getExpAsString))      
      | getExpAsString(Absyn.STRUCTexp(ctor, exp_star, _)) =
				(print_ctor_opt(ctor)^print_parens_comma(exp_star, getExpAsString))				
      | getExpAsString(Absyn.IDENTexp(longid, _, _)) = (print_longid(*_info*)(longid))

    fun getExpsFromExp(x as Absyn.VARexp(longid,info)) = [x]
	  | getExpsFromExp(Absyn.LITexp(lit, infoLIT)) = []
      | getExpsFromExp(Absyn.CONexp(longcon, infoCON)) = [Absyn.CONexp(longcon, infoCON)]
      | getExpsFromExp(Absyn.STRUCTexp(ctor, exp_star, _)) = getExpsFromExpStar exp_star
      | getExpsFromExp(id as Absyn.IDENTexp(_, _, _)) = [id]
	and getExpsFromExpStar(foo::foo_star) = getExpsFromExp(foo) @ (getExpsFromExpStar foo_star)
	  | getExpsFromExpStar(nil) = []

    fun getExpsFromPat(Absyn.WILDpat _) = []
      | getExpsFromPat(Absyn.LITpat(lit, infoLIT)) = []
      | getExpsFromPat(Absyn.CONpat(longcon, infoCON)) = [Absyn.CONexp(longcon, infoCON)]
      | getExpsFromPat(Absyn.STRUCTpat(ctor, pat_star, _, _)) = getExpsFromPatStar pat_star
      | getExpsFromPat(Absyn.BINDpat(var, pat, infoBIND)) = 
			Absyn.IDENTexp(Absyn.LONGID(NONE, var, Absyn.dummyInfo), 
				ref(Absyn.STRUCTexp(NONE, [], Absyn.dummyInfo)),
				infoBIND)::(getExpsFromPat pat)
      | getExpsFromPat(Absyn.IDENTpat(id as Absyn.IDENT(_, infoID), _, infoIDENT)) = 
			[Absyn.IDENTexp(Absyn.LONGID(NONE, id, infoID), 
			 ref(Absyn.STRUCTexp(NONE, [], Absyn.dummyInfo)),
			 infoIDENT)]
	  | getExpsFromPat(Absyn.NAMEDpat(_, pat, _)) = getExpsFromPat(pat)
	and getExpsFromPatStar(foo::foo_star) = getExpsFromPat(foo) @ (getExpsFromPatStar foo_star)
	  | getExpsFromPatStar(nil) = []
					    	
    fun getAllResultExps(Absyn.RETURN(exps, _)) = ( exps )
      | getAllResultExps(Absyn.FAIL (info)) = 
			[Absyn.LITexp(
				Absyn.SCONlit("fail", info),
				info)]
				
    fun getResultExps(Absyn.RETURN(exps, _)) = ( getExpsFromExpStar exps )
      | getResultExps(Absyn.FAIL (info)) = 
			[Absyn.LITexp(
				Absyn.SCONlit("fail", info),
				info)]
				
    (* ------------------------------------------------------ *)
    (* ------------------------------------------------------ *)
    fun instrument(fileName, m as Absyn.MODULE(Absyn.INTERFACE({source,...}, _), _, _)) =
    (* ------------------------------------------------------ *)
    let 
								  
    fun getStrNr(x) =
		if (x < 10) 
		then Int.toString(0)^Int.toString(x)
		else Int.toString(x)
		
	fun getPush(direction, arity)
		= Absyn.LONGID(
			modidRML, 
			Absyn.rmlIdent ("debug_push_"^direction^getStrNr(arity)), 
			Absyn.dummyInfo)
	
	fun makeTuple(x, y) = Absyn.STRUCTexp(NONE, x :: y, Absyn.dummyInfo)
	fun makeSingleTuple(x) = Absyn.STRUCTexp(NONE, x, Absyn.dummyInfo)
	
	fun addDummyParams(foo, nelts) =
		if (nelts = 0) 
		then foo 
		else (* add one more to the list *)
		addDummyParams(foo @ [makeTuple(dummyDebugVar, [dummyDebugVar])], nelts-1) 

	(* check if we have more than 32 parameters to show *)
    fun checkParams(foo) = 
    let val llength = List.length foo
    in
		if (llength > 32)
		then
			bug("checkParams: this debugger handles only 32 variables between goals")
		else
			[makeSingleTuple(addDummyParams(foo, 32-llength))]
	end      		

	fun getExpAST(exp) = 
		Absyn.LITexp(Absyn.SCONlit(getExpAsString(exp), Absyn.dummyInfo),
			Absyn.dummyInfo)

	fun makeTupleList(nil, func) = []
		| makeTupleList(foo::nil, func) = [makeTuple(func(foo), [foo])]
		| makeTupleList(foo::rest, func) = 
			[makeTuple(func(foo), [foo])] @ makeTupleList(rest, func) 						
		
	fun makeParamList(nil, func) = []
		| makeParamList(foo::nil, func) = [func(foo), foo]
		| makeParamList(foo::rest, func) = 
			[func(foo), foo] @ makeParamList(rest, func) 							
		
    fun printVars(foo, direction) =
    let val llength = List.length foo
    in
		if llength = 0
		then Absyn.CALLgoal(getPush(direction, 1), [dummyDebugVar, dummyDebugVar], 
			 [], ref [], Absyn.dummyInfo)
		else
			if llength < 17
			then Absyn.CALLgoal(getPush(direction, llength), makeParamList(foo, getExpAST), 
				[], ref [], Absyn.dummyInfo)
			else Absyn.ANDgoal(
					printVars(List.take(foo, 16), direction),
					printVars(List.drop(foo, 16), direction),
					Absyn.dummyInfo)
	end

    fun printPats(foo, direction) = printVars(getExpsFromPatStar foo, direction)

    fun getPositionFromResult(Absyn.RETURN(exps, locRETURN)) = (locRETURN)
      | getPositionFromResult(Absyn.FAIL (locFAIL)) = (locFAIL)
						
	fun makeExp(
			relIdent as Absyn.IDENT(stringIdent, _), 
			Absyn.INFO(sp, ep), 
			strGoal) =
	let val {fileName, sline, scolumn, eline, ecolumn} = Absyn.Source.getLoc(source, sp, ep)
	in
	[Absyn.LITexp(Absyn.SCONlit(fileName, Absyn.dummyInfo), Absyn.dummyInfo),
	 Absyn.LITexp(Absyn.ICONlit(sline, Absyn.dummyInfo), Absyn.dummyInfo),
	 Absyn.LITexp(Absyn.ICONlit(scolumn, Absyn.dummyInfo), Absyn.dummyInfo),		 
	 Absyn.LITexp(Absyn.ICONlit(eline,Absyn.dummyInfo), Absyn.dummyInfo),
	 Absyn.LITexp(Absyn.ICONlit(ecolumn,Absyn.dummyInfo), Absyn.dummyInfo),		 
	 Absyn.LITexp(Absyn.SCONlit(stringIdent, Absyn.dummyInfo), Absyn.dummyInfo),
	 Absyn.LITexp(Absyn.SCONlit(strGoal, Absyn.dummyInfo), Absyn.dummyInfo)]
	end 
		 
	(* adrpo added 2004-11-20 *)
	(* only instrument in front, needed for last goal in a rule
	   instrumentation which should not be instrumented after the goal (tail recursiveness) *)
    fun instrumentGoalOnlyInFront(
   		Absyn.CALLgoal(longid, exp_star, pat_star, pat_star_ref, infoCALL), 
		relIdent, result) = 
	    Absyn.ANDgoal(
			if List.length exp_star > 0
			then
			Absyn.ANDgoal(
				printVars(getExpsFromExpStar exp_star, "in"), (* push in variables of next goal *)
				Absyn.CALLgoal(
					debug, 
					makeExp(
						relIdent, 
						infoCALL,
						Control.selectCompilerMessage(
						"call:"^print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)^" => "^
						print_parens_comma(pat_star, getPatAsString),
						"call:"^print_parens_comma(pat_star, getPatAsString)^" = "^
						print_longid(longid)^print_parens_comma(exp_star, getExpAsString))
						), 
						[], 
						ref [], 
						Absyn.dummyInfo), 
				Absyn.dummyInfo)
			else
				Absyn.CALLgoal(
					debug, 
					makeExp(
						relIdent, 
						infoCALL,
						Control.selectCompilerMessage(
						"call:"^
						print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)^" => "^
						print_parens_comma(pat_star, getPatAsString),
						"call:"^
						print_parens_comma(pat_star, getPatAsString)^" = "^
						print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)
						)), 
						[],
						ref [], 
						Absyn.dummyInfo),
			Absyn.CALLgoal(longid, exp_star, pat_star, pat_star_ref, infoCALL), 
			Absyn.dummyInfo) 
      | instrumentGoalOnlyInFront(Absyn.EQUALgoal(
								var1, 
								exp2, 
								infoEQUAL), 
						relIdent, result) =
	    Absyn.ANDgoal(
		 Absyn.ANDgoal(
			printVars([
				Absyn.IDENTexp(Absyn.LONGID(NONE, var1, Absyn.dummyInfo), 
				ref(Absyn.STRUCTexp(NONE,[], Absyn.dummyInfo)), 
			Absyn.dummyInfo), exp2], "in"),
				Absyn.CALLgoal(debug, 
				makeExp(relIdent, infoEQUAL, 
					Control.selectCompilerMessage("equal:","equality:")
					^print_ident(var1)^" = "^getExpAsString(exp2)), 
				[],	ref [], Absyn.dummyInfo),
			Absyn.dummyInfo),				
		 Absyn.EQUALgoal(var1, exp2, infoEQUAL),
		 Absyn.dummyInfo)
      | instrumentGoalOnlyInFront(
			Absyn.LETgoal(
				pat, 
				exp,  
				infoLET), 
				relIdent, result) =
	    Absyn.ANDgoal(
			Absyn.CALLgoal(
				debug, 
				makeExp(relIdent, infoLET, 
				Control.selectCompilerMessage("let:","assignment:")
				^getPatAsString(pat)^" = "^getExpAsString(exp)), 
				[],
				ref [], 
				Absyn.dummyInfo),
			Absyn.LETgoal(pat, exp, infoLET), 
			Absyn.dummyInfo )
      | instrumentGoalOnlyInFront(
			Absyn.NOTgoal(
				g, 
				infoNOT), 
				relIdent, result) =
	    Absyn.ANDgoal(
			Absyn.CALLgoal(
				debug, 
				makeExp(relIdent, infoNOT, 
				Control.selectCompilerMessage("not:","failure:")), 
				[],
				ref [],
				Absyn.dummyInfo),
			Absyn.NOTgoal(
				(* instrumentGoalOnlyInFront(g, relIdent, result), *)
				g,
				infoNOT),
			Absyn.dummyInfo)
	  (* this shouldn't happen, but let's be safe *)
      | instrumentGoalOnlyInFront(Absyn.ANDgoal(g1,g2,infoAND), relIdent, result) =  
			(
			 bug("instrumentGoalOnlyInFront: [warning] ANDgoal found when it shouldn't be there!\n"); 
			 Absyn.ANDgoal(g1, g2,Absyn.dummyInfo)
			)				  		 

    fun instrumentGoal(
		Absyn.CALLgoal(longid, exp_star, pat_star, pat_star_ref, infoCALL), 
		relIdent, result) =
	    Absyn.ANDgoal(
			if (List.length exp_star > 0)
			then 
			Absyn.ANDgoal(
				printVars(getExpsFromExpStar exp_star,"in"),			
				Absyn.CALLgoal(
					debug, 
					makeExp(
						relIdent, 
						infoCALL,
						Control.selectCompilerMessage(
						"call:"^print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)^" => "^
						print_parens_comma(pat_star, getPatAsString),
						"call:"^print_parens_comma(pat_star, getPatAsString)^" = "^
						print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)
						)), 
						[],
						ref [],
						Absyn.dummyInfo), 
				Absyn.dummyInfo)
			else 
				Absyn.CALLgoal(
					debug, 
					makeExp(
						relIdent, 
						infoCALL,
						Control.selectCompilerMessage(
						"call:"^print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)^" => "^
						print_parens_comma(pat_star, getPatAsString),
						"call:"^print_parens_comma(pat_star, getPatAsString)^" = "^
						print_longid(longid)^
						print_parens_comma(exp_star, getExpAsString)
						)), 
						[],
						ref [],
						Absyn.dummyInfo),
			(if List.length pat_star > 0
			then Absyn.ANDgoal(
					Absyn.CALLgoal(longid, exp_star, pat_star, pat_star_ref, infoCALL),
						printPats(pat_star,"out"),
						Absyn.dummyInfo)
			else Absyn.CALLgoal(longid, exp_star, pat_star, pat_star_ref, infoCALL)), 
			Absyn.dummyInfo )
      | instrumentGoal(Absyn.EQUALgoal(
								var1, 
								exp2, 
								infoEQUAL), 
						relIdent, result) =
	    Absyn.ANDgoal(
		 Absyn.ANDgoal(
			printVars([
				Absyn.IDENTexp(Absyn.LONGID(NONE, var1, Absyn.dummyInfo), 
				ref(Absyn.STRUCTexp(NONE,[], Absyn.dummyInfo)), 
				Absyn.dummyInfo), exp2], "in"),
			Absyn.CALLgoal(debug, 
				makeExp(relIdent, infoEQUAL, 
					Control.selectCompilerMessage("equal:","equality:")^
					print_ident(var1)^" = "^getExpAsString(exp2)), 
				[],	ref[], Absyn.dummyInfo),
			Absyn.dummyInfo),				
		 Absyn.EQUALgoal(var1, exp2, infoEQUAL),
		 Absyn.dummyInfo)
      | instrumentGoal(
			Absyn.LETgoal(
				pat, 
				exp, 
				infoLET), 
				relIdent, result) =
	    Absyn.ANDgoal(
			Absyn.CALLgoal(
				debug, 
				makeExp(relIdent, infoLET, 
				Control.selectCompilerMessage("let:","assignment:")
				^getPatAsString(pat)^" = "^getExpAsString(exp)), 
				[],
				ref [],
				Absyn.dummyInfo),
			Absyn.ANDgoal(
				Absyn.LETgoal(pat, exp, infoLET), 
				Absyn.ANDgoal(
					printPats([pat], "out"), 
					printVars(getExpsFromExp exp, "out"), Absyn.dummyInfo),
				Absyn.dummyInfo ), 
			Absyn.dummyInfo )
      | instrumentGoal(
			Absyn.NOTgoal(
				g, 
				infoNOT), 
				relIdent, result) =
	    Absyn.ANDgoal(
			Absyn.CALLgoal(
				debug, 
				makeExp(relIdent, infoNOT, Control.selectCompilerMessage("not:","failure:")), 
				[],
				ref [], 
				Absyn.dummyInfo),
			Absyn.NOTgoal(
				(*instrumentGoal(g, relIdent, result), *)
				g,
				infoNOT),
			Absyn.dummyInfo)	  
      | instrumentGoal(Absyn.ANDgoal(g1, g2 as Absyn.ANDgoal(g3, g4, _), infoAND), relIdent, result) =
      		Absyn.ANDgoal(
      			instrumentGoal(g1, relIdent, result), 
      			instrumentGoal(g2, relIdent, result),
      			infoAND)      		
      | instrumentGoal(
			Absyn.ANDgoal(
				g1, 
				g2, 
				infoAND), 
				relIdent, 
				result) =  
			Absyn.ANDgoal(
      				instrumentGoal(g1, relIdent, result), 
					instrumentGoalOnlyInFront(g2, relIdent, result),
					Absyn.dummyInfo)
		(* adrpo 2004-11-20 result shouldn't be displayed as it will interfere with recursive tailcall 
		   XXX fix to be only if last is Absyn.CALLgoal(relname, xxx)  			
			let val locRESULT = getPositionFromResult(result)
				val result_exp = getResultExps result
			in 
      			Absyn.ANDgoal(
      				instrumentGoal(g1, relIdent, result), 
      				Absyn.ANDgoal(
      					instrumentGoal(g2, relIdent, result),
      					Absyn.ANDgoal(
							Absyn.CALLgoal(
								debug, 
								makeExp(
									relIdent, 
									locRESULT, 
									"return:"^print_parens_comma(result_exp, getExpAsString)), 
									[], 
									ref [],
									Absyn.dummyInfo),
							printVars(result_exp),
							Absyn.dummyInfo),
						Absyn.dummyInfo),
      				Absyn.dummyInfo)
			end
			*)
			
    fun instrumentGoals(NONE, relIdent as Absyn.IDENT(stringIdent, _), locGOAL, pat_star, result) = 
		let val resultExps = getResultExps result 
		in
		SOME(
			 if List.length resultExps > 0
			 then
			 Absyn.ANDgoal(
				if List.length pat_star > 0 andalso List.length (getExpsFromPatStar pat_star) > 0
				then
				Absyn.ANDgoal(
					printPats(pat_star,"in"), 	
					Absyn.CALLgoal(
						debug, 
						makeExp(relIdent, locGOAL,
							 Control.selectCompilerMessage("axiom:","case:")^
							 stringIdent^
							 print_parens_comma(pat_star, getPatAsString)^
							 Control.selectCompilerMessage(" => "," then ")^
							 print_parens_comma(getAllResultExps result, getExpAsString)), 
						[],
						ref [],
						Absyn.dummyInfo),
					Absyn.dummyInfo)
				else
					Absyn.CALLgoal(
						debug, 
						makeExp(relIdent, locGOAL, 
							 Control.selectCompilerMessage("axiom:","case:")^
							 stringIdent^
							 print_parens_comma(pat_star, getPatAsString)^
							 Control.selectCompilerMessage(" => "," then ")^
							 print_parens_comma(getAllResultExps result, getExpAsString)), 
						[],
						ref [], 
						Absyn.dummyInfo),
				printVars(getExpsFromExpStar resultExps, "out"),
				Absyn.dummyInfo)
			else
				if List.length pat_star > 0
				then
				Absyn.ANDgoal(
					printPats(pat_star,"in"), 	
					Absyn.CALLgoal(
						debug, 
						makeExp(relIdent, locGOAL, 
							 Control.selectCompilerMessage("axiom:","case:")^
							 stringIdent^
							 print_parens_comma(pat_star, getPatAsString)^
							 Control.selectCompilerMessage(" => "," then ")^
							 print_parens_comma(getAllResultExps result, getExpAsString)), 
						[],
						ref [], 
						Absyn.dummyInfo),
					Absyn.dummyInfo)
				else
					Absyn.CALLgoal(
						debug, 
						makeExp(relIdent, locGOAL, 
							 Control.selectCompilerMessage("axiom:","case:")^
							 stringIdent^
							 print_parens_comma(pat_star, getPatAsString)^
							 Control.selectCompilerMessage(" => "," then ")^
							 print_parens_comma(getAllResultExps result, getExpAsString)), 
						[],
						ref [], 
						Absyn.dummyInfo)
		)
		end
	  | instrumentGoals(SOME goal, relIdent, locGOAL, pat_star, result) = 
		SOME(instrumentGoal(goal, relIdent, result))
			(*Absyn.ANDgoal(
					printPats(pat_star, "in"),		
					instrumentGoal(goal, relIdent, result),
					Absyn.dummyInfo))*)

    fun instrumentClause(
			Absyn.CLAUSE1(
				g_opt, 
				id,
				pat_star, result,
				ref_pat_star,
				vars,
				infoCLAUSE1), 
				relIdent) =
		Absyn.CLAUSE1(
			instrumentGoals(
				g_opt, 
				relIdent, 
				infoCLAUSE1, 
				pat_star, 
				result), 
			id, pat_star, result, ref_pat_star, vars, infoCLAUSE1)
      | instrumentClause(Absyn.CLAUSE2(cl1,cl2, infoCLAUSE2), relIdent) =
	    Absyn.CLAUSE2(
			instrumentClause(cl1, relIdent), 
			instrumentClause(cl2, relIdent),
			infoCLAUSE2)
       
    fun instrumentRel(Absyn.RELBIND(ident, ty_op, clause, x, y, infoRELBIND)) = 
       Absyn.RELBIND(ident, ty_op, instrumentClause(clause, ident), x, y, infoRELBIND)
    
    fun instrumentDec(Absyn.RELdec(rels, infoREL)) = Absyn.RELdec(map instrumentRel rels, infoREL)
      | instrumentDec dec = dec 

    fun instrumentModule(Absyn.MODULE(interface, declarations, infoMODULE)) = 
		Absyn.MODULE(interface, map instrumentDec declarations, infoMODULE)
		
 in 	let val m = if !Control.dumpInterface = false orelse !Control.dumpDepends = false 
	                then instrumentModule m else m
		in
			m
		end
 end 
end (* functor InstrumentFn *)
