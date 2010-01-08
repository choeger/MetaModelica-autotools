(* cps/cps_usages.sml *)

functor CPSUsagesFn(structure Util : UTIL
		    structure CPS : CPS
		    structure CPSUseful : CPS_USEFUL
		    sharing CPS = CPSUseful.CPS
		      ) : CPS_USAGES =
  struct

    structure CPS = CPS

    fun zap_var(CPS.VAR{uses,...}) = uses := 0
    fun use_var(CPS.VAR{uses,...}) = uses := !uses + 1

    fun zap_lk(CPS.FClk) = ()
      | zap_lk(CPS.SClk{v_tvs}) = List.app zap_var v_tvs

    fun zap_def(CPS.DEF{uses,...}) = if !uses > 0 then uses := 0 else ()

    fun use_proc(CPS.LOCAL_REL(CPS.DEF{uses,...})) =
	  if !uses >= 0 then uses := !uses + 1 else ()
      | use_proc(CPS.EXTERN_REL _) = ()

    fun update_lit(CPS.PROClit(proc)) = use_proc proc
      | update_lit(CPS.STRUCTlit(_, lit_star, _)) = List.app update_lit lit_star
      | update_lit(_) = ()

    fun update_te'(CPS.VARte(var)) = use_var var
      | update_te'(CPS.LAMte{kind,body,...}) = (zap_lk kind; update_exp body)
      | update_te'(CPS.QUOTEte(lit)) = update_lit lit

    and update_te te = update_te'(CPS.getTE te)

    and update_prim(CPS.MARKERp) = ()
      | update_prim(CPS.MKSTRUCTp(_, te_star)) = List.app update_te te_star
      | update_prim(CPS.UNARYp(_, te)) = update_te te
      | update_prim(CPS.BINARYp(_, te1, te2)) = (update_te te1; update_te te2)

    and update_exp'(CPS.AppFCe{fc=te_fc, name=name, pos=pos}) = update_te te_fc
      | update_exp'(CPS.AppSCe{sc, args, name, pos}) =
	  (update_te sc; List.app update_te args)
      | update_exp'(CPS.AppPVe{pv, args, fc, sc, name, pos}) =
	  (update_te pv; List.app update_te args;
	   update_te fc; update_te sc)
      | update_exp'(CPS.LetLABe(CPS.LAB{uses,bvars,body,...},exp)) =
	  (uses := 0; List.app zap_var bvars; update_exp body; update_exp exp)
      | update_exp'(CPS.AppLABe(CPS.LAB{uses,...}, t_star)) =
	  (uses := !uses + 1; List.app update_te t_star)
      | update_exp'(CPS.RESTOREe(te, exp)) = (update_te te; update_exp exp)
      | update_exp'(CPS.LETe(var, te, exp)) =
	  (update_te te; zap_var var; update_exp exp)
      | update_exp'(CPS.PRIMe(var, prim, exp)) =
	  (update_prim prim; zap_var var; update_exp exp)
      | update_exp'(CPS.SWITCHe(te, cases, default)) =
	  (update_te te; List.app update_case cases; updateDefault default)

    and updateDefault(NONE) = ()
      | updateDefault(SOME exp) = update_exp exp

    and update_case(_, exp) = update_exp exp

    and update_exp(CPS.EXP r) = update_exp'(!r)

    fun update_def(CPS.DEF{v_tvs,v_fc,v_sc,body,...}) =
      (List.app zap_var v_tvs; zap_var v_fc; zap_var v_sc; update_exp body)

    fun update module =
      let val (module as CPS.MODULE{defines,...}) = CPSUseful.useful module
      in
		List.app zap_def defines;
		List.app update_def defines;
		module
      end

  end (* functor CPSUsagesFn *)
