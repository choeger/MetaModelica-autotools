(* cps/cps_useful.sml *)

functor CPSUsefulFn(structure Util : UTIL
		    structure CPS : CPS
		      ) : CPS_USEFUL =
  struct

    structure CPS = CPS
	structure ConRep = CPS.ConRep
    

    fun zap_def(CPS.DEF{uses,...}) = if !uses > 0 then uses := 0 else ()

    fun useful_proc(CPS.LOCAL_REL(CPS.DEF{uses,...})) =
	  if !uses = 0 then uses := 1 else ()
      | useful_proc(CPS.EXTERN_REL _) = ()

    fun useful_lit(CPS.CONSTlit _) = ()
      | useful_lit(CPS.STRUCTlit(_, lit_star, _)) = List.app useful_lit lit_star
      | useful_lit(CPS.PROClit(proc)) = useful_proc proc
      | useful_lit(CPS.EXTERNlit _) = ()

    fun useful_te'(CPS.VARte _) = ()
      | useful_te'(CPS.LAMte{body,...}) = useful_exp body
      | useful_te'(CPS.QUOTEte(lit)) = useful_lit lit

    and useful_te te = useful_te'(CPS.getTE te)

    and useful_prim(CPS.MARKERp) = ()
      | useful_prim(CPS.MKSTRUCTp(_, te_star)) = List.app useful_te te_star
      | useful_prim(CPS.UNARYp(_, te)) = useful_te te
      | useful_prim(CPS.BINARYp(_, te1, te2)) = (useful_te te1; useful_te te2)

    and useful_exp'(CPS.AppFCe{fc=te_fc,name=name,pos=pos}) = useful_te te_fc
      | useful_exp'(CPS.AppSCe{sc, args, name, pos}) =
	  (useful_te sc; List.app useful_te args)
      | useful_exp'(CPS.AppPVe{pv, args, fc, sc, name, pos}) =
	  (useful_te pv; List.app useful_te args; useful_te fc; useful_te sc)
      | useful_exp'(CPS.LetLABe(CPS.LAB{body,...},exp)) =
	  (useful_exp body; useful_exp exp)
      | useful_exp'(CPS.AppLABe(_, te_star)) = List.app useful_te te_star
      | useful_exp'(CPS.RESTOREe(te, exp)) = (useful_te te; useful_exp exp)
      | useful_exp'(CPS.LETe(_, te, exp)) = (useful_te te; useful_exp exp)
      | useful_exp'(CPS.PRIMe(_, prim, exp)) = (useful_prim prim; useful_exp exp)
      | useful_exp'(CPS.SWITCHe(te, cases, default)) =
	  (useful_te te; List.app useful_case cases; usefulDefault default)

    and usefulDefault(NONE) = ()
      | usefulDefault(SOME exp) = useful_exp exp

    and useful_case(_, exp) = useful_exp exp

    and useful_exp(CPS.EXP r) = useful_exp'(!r)

    fun useful_def(CPS.DEF{body,...}) = useful_exp body

    fun useful_defs alldefs =
      let fun split([], useful, [], _) = useful		(*WE'RE DONE*)
	    | split([], useful, toscan, unknown) = scan(toscan, useful, unknown)
	    | split((def as CPS.DEF{uses,...})::tosplit, useful, toscan, unknown) =
		if !uses = 1 then split(tosplit, useful, def::toscan, unknown)
		else split(tosplit, useful, toscan, def::unknown)
	  and scan([], useful, unknown) = split(unknown, useful, [], [])
	    | scan(def::toscan, useful, unknown) =
		(useful_def def; scan(toscan, def::useful, unknown))
	  fun init([], toscan, unknown) = scan(toscan, [], unknown)
	    | init((def as CPS.DEF{uses,...})::defs, toscan, unknown) =
		if !uses < 0 then init(defs, def::toscan, unknown)
		else init(defs, toscan, def::unknown)
      in
		init(alldefs, [], [])
      end

    fun useful_value(_, lit) = useful_lit lit

    fun useful(CPS.MODULE{name,ctors,xmods,values,defines,source}) =
      let val _ = List.app zap_def defines
	  val _ = List.app useful_value values
	  val defines = useful_defs defines
      in
		CPS.MODULE{name=name,ctors=ctors,xmods=xmods,values=values,defines=defines,source=source}
      end

  end (* functor CPSUsefulFn *)
