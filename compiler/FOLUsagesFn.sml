(* fol/fol-usages.sml *)

functor FOLUsagesFn(structure Util : UTIL
		    structure FOL : FOL
		      ) : FOL_USAGES =
  struct

    structure FOL = FOL

    fun zapVar var =
      let val FOL.VAR({uses,...}, _) = FOL.deref var
      in
		uses := 0
      end

    fun incVar var =
      let val FOL.VAR({uses,...}, _) = FOL.deref var
      in
		uses := !uses + 1
      end

    fun doPat'(FOL.STRUCTpat(_, pats)) = List.app doPat pats
      | doPat'(_) = ()
    and doPat(FOL.PAT(var, pat)) = (zapVar var; doPat' pat)

    fun doVarRef(FOL.BVAR var) = incVar var
      | doVarRef(_) = ()

    fun doExp(FOL.VARexp vref) = doVarRef vref
      | doExp(FOL.STRUCTexp(_, exps)) = List.app doExp exps
      | doExp(_) = ()

    fun doMRule(var, pat) = (incVar var; doPat pat)

    fun doConj(FOL.CALL(vref, exps, vars, _)) =
	  (doVarRef vref; List.app doExp exps; List.app zapVar vars)
      | doConj(FOL.MATCH(mrules, _)) = List.app doMRule mrules
      | doConj(FOL.EQUAL(var, exp, _)) = (doExp exp; incVar var)
      | doConj(FOL.BIND(var, exp, _)) = (doExp exp; zapVar var)
      | doConj(FOL.NOT(conj, _)) = doConj conj
      | doConj(FOL.AND(c1, c2, _)) = (doConj c1; doConj c2)
      | doConj(FOL.IF(c1, c2, c3, _)) = (doConj c1; doConj c2; doConj c3)

    fun doDisj(FOL.RETURN(exps, _)) = List.app doExp exps
      | doDisj(FOL.FAIL(_)) = ()
      | doDisj(FOL.ORELSE(d1, d2, _)) = (doDisj d1; doDisj d2)
      | doDisj(FOL.ANDTHEN(c1, d2, _)) = (doConj c1; doDisj d2)
      | doDisj(FOL.COND(c1, d2, d3, _)) = (doConj c1; doDisj d2; doDisj d3)
      | doDisj(FOL.CASE(vars, cases, _)) =
	  (List.app incVar vars; List.app doCase cases)

    and doCase(pats, disj) = (List.app doPat pats; doDisj disj)

    fun doRel(FOL.REL(_,vars,disj, _)) = (List.app zapVar vars; doDisj disj)

    fun doDec(FOL.RELdec rels) = List.app doRel rels
      | doDec(_) = ()

    fun update(FOL.MODULE(_, decs, _)) = List.app doDec decs

  end (* functor FOLUsagesFn *)
