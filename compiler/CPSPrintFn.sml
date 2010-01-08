(* cps/cps_print.sml *)

functor CPSPrintFn(structure MakeString : MAKESTRING
		   structure Util : UTIL
		   structure PP : PP
		   structure CPS : CPS
		     ) : CPS_PRINT =
  struct

    structure CPS = CPS
    structure ConRep = CPS.ConRep

    fun prStr(pp, str) = PP.emit(pp, PP.STRING str)
    fun prBlank(pp) = PP.emit(pp, PP.BREAK{blankSpace=1, offset=0})
    fun prNewLine(pp) = PP.emit(pp, PP.lineBreak)
    fun prInt(pp, i) = prStr(pp, MakeString.icvt i)

    fun pr_var(pp, CPS.VAR{tag,uses,name,...}) =
      prStr(pp, "v" ^ (MakeString.icvt tag) ^ ":" ^(CPS.longIdentName name)^":"^(MakeString.icvt(!uses)))

    fun pr_space_var pp var = (prBlank(pp); pr_var(pp, var))

    fun pr_longid(pp, x as ConRep.LONGID{module,name}) = prStr(pp, CPS.longIdentName x)

    fun pr_proc(pp, CPS.EXTERN_REL(longid,_)) = pr_longid(pp, longid)
      | pr_proc(pp, CPS.LOCAL_REL(CPS.DEF{name,...})) = pr_longid(pp, name)

    fun pr_string(pp, str) = prStr(pp, MakeString.scvt str)

    fun pr_constant(pp, CPS.INTcon i) = prInt(pp, i)
      | pr_constant(pp, CPS.HDRcon{len,con}) =
	  (prStr(pp, "(STRUCTHDR "); prInt(pp, len);
	   prStr(pp," "); prInt(pp,con); prStr(pp, ")"))
      | pr_constant(pp, CPS.REALcon r) = prStr(pp, MakeString.rcvt r)
      | pr_constant(pp, CPS.STRINGcon s) = pr_string(pp, s)

    fun pr_literal(pp, CPS.CONSTlit c)	= pr_constant(pp, c)
      | pr_literal(pp, CPS.STRUCTlit(con, lits, name)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=2});
	   prStr(pp, "#(");
	   prInt(pp, con);
	   List.app (pr_space_literal pp) lits;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_literal(pp, CPS.PROClit p) = (prStr(pp, ","); pr_proc(pp, p))
      | pr_literal(pp, CPS.EXTERNlit longid) = (prStr(pp,","); pr_longid(pp,longid))

    and pr_space_literal pp lit = (prBlank(pp); pr_literal(pp, lit))

    fun lamkind(CPS.FClk)	= "fc"
      | lamkind(CPS.SClk _)	= "sc"

    fun pr_lamkind(pp, kind, tag:int) =
      (prStr(pp, "lambda \"" ^ (lamkind kind) ^ (MakeString.icvt tag) ^ "\" ");
       prStr(pp, "(");
       PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=0});
       (case kind
	  of CPS.FClk	=> ()
	   | CPS.SClk{v_tvs} => List.app (pr_space_var pp) v_tvs);
       prStr(pp, ")");
       PP.emit(pp, PP.END))

    fun unary(CPS.FETCH off)	= "fetch "^(MakeString.icvt off)
      | unary(CPS.BOOL_NOT)	= "bool_not"
      | unary(CPS.INT_NEG)	= "int_neg"
      | unary(CPS.INT_ABS)	= "int_abs"

    fun binary(CPS.EQUAL)	= "equal"
      | binary(CPS.BOOL_AND)	= "bool_and"
      | binary(CPS.BOOL_OR)	= "bool_or"
      | binary(CPS.INT_ADD)	= "int_add"
      | binary(CPS.INT_SUB)	= "int_sub"
      | binary(CPS.INT_MUL)	= "int_mul"
      | binary(CPS.INT_DIV)	= "int_div"
      | binary(CPS.INT_MOD)	= "int_mod"
      | binary(CPS.INT_MAX)	= "int_max"
      | binary(CPS.INT_MIN)	= "int_min"
      | binary(CPS.INT_LT)	= "int_lt"
      | binary(CPS.INT_LE)	= "int_le"
      | binary(CPS.INT_EQ)	= "int_eq"
      | binary(CPS.INT_NE)	= "int_ne"
      | binary(CPS.INT_GE)	= "int_ge"
      | binary(CPS.INT_GT)	= "int_gt"

    fun pr_te'(pp, CPS.VARte var) = pr_var(pp, var)
      | pr_te'(pp, CPS.LAMte{tag,kind,body,...}) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=2});
	   prStr(pp, "(");
	   pr_lamkind(pp, kind, tag);
	   prBlank(pp);
	   pr_exp(pp,body);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_te'(pp, CPS.QUOTEte lit) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=8});
	   prStr(pp, "`");
	   pr_literal(pp, lit);
	   PP.emit(pp, PP.END))

    and pr_te(pp, te) = pr_te'(pp, CPS.getTE te)

    and pr_blank_te pp te = (prBlank(pp); pr_te(pp, te))

    and pr_prim(pp, prim) =
      (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=2});
       prStr(pp, "(");
       (case prim
	  of CPS.MARKERp	=> prStr(pp, "rml_marker")
	   | CPS.MKSTRUCTp(con, te_star)=>
	      (prStr(pp, "rml_mkstruct"); prBlank pp; prInt(pp, con);
	       List.app (pr_blank_te pp) te_star)
	   | CPS.UNARYp(unop,te)	=>
	      (prStr(pp, unary unop); pr_blank_te pp te)
	   | CPS.BINARYp(binop, te1, te2)	=>
	      (prStr(pp, binary binop); pr_blank_te pp te1; pr_blank_te pp te2));
       prStr(pp, ")");
       PP.emit(pp, PP.END))

    and pr_exp'(pp, CPS.AppFCe{fc=t_fc,name=name,pos=pos}) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=5});
	   prStr(pp, "(@fc ");
	   pr_te(pp, t_fc);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.AppSCe{sc,args,name,pos}) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=5});
	   prStr(pp, "(@sc ");
	   pr_te(pp, sc);
	   List.app (pr_blank_te pp) args;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.AppPVe{pv,args,fc,sc,name,pos}) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=5});
	   prStr(pp, "(@pv ");
	   pr_te(pp, pv);
	   List.app (pr_blank_te pp) args;
	   prBlank(pp);
	   pr_te(pp, fc);
	   prBlank(pp);
	   pr_te(pp, sc);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.LetLABe(CPS.LAB{tag, bvars, body, ...}, exp)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=0});
	   prStr(pp, "(let ((");
	   PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
	   prStr(pp, "lab"^(MakeString.icvt tag));
	   prBlank(pp);
	   PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=2});
	   prStr(pp, "(label \"" ^ (MakeString.icvt tag) ^ "\" (");
	   PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=0});
	   List.app (pr_space_var pp) bvars;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END);
	   prBlank(pp);
	   pr_exp(pp, body);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END);
	   prStr(pp, "))");
	   PP.emit(pp, PP.END);
	   prBlank(pp);
	   pr_exp(pp, exp);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.AppLABe(CPS.LAB{tag, ...}, t_star)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=8});
	   prStr(pp, "(@label ");
	   prStr(pp, "lab"^(MakeString.icvt tag));
	   List.app (pr_blank_te pp) t_star;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.RESTOREe(te, exp)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=2});
	   prStr(pp, "(begin");
	   prBlank(pp);
	   PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=2});
	   prStr(pp, "(rml_unwind");
	   pr_blank_te pp te;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END);
	   prBlank(pp);
	   pr_exp(pp, exp);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.LETe(var, te, exp)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=0});
	   prStr(pp, "(let ((");
	   PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
	   pr_var(pp, var);
	   prBlank(pp);
	   pr_te(pp, te);
	   prStr(pp, "))");
	   PP.emit(pp, PP.END);
	   prBlank(pp);
	   pr_exp(pp, exp);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.PRIMe(var, prim, exp)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=0});
	   prStr(pp, "(let ((");
	   PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
	   pr_var(pp, var);
	   prBlank(pp);
	   pr_prim(pp, prim);
	   prStr(pp, "))");
	   PP.emit(pp, PP.END);
	   prBlank(pp);
	   pr_exp(pp, exp);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))
      | pr_exp'(pp, CPS.SWITCHe(te, cases, default)) =
	  (PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=2});
	   prStr(pp, "(switch ");
	   pr_te(pp, te);
	   List.app (pr_case pp) cases;
	   prDefault pp default;
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))

    and prDefault _ NONE = ()
      | prDefault pp (SOME exp) =
	  (prBlank(pp);
	   PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
	   prStr(pp, "(else");
	   prBlank(pp);
	   pr_exp(pp, exp);
	   prStr(pp, ")");
	   PP.emit(pp, PP.END))

    and pr_case pp (const, exp) =
      (prBlank(pp);
       PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
       prStr(pp, "(");
       PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=1});
       prStr(pp, "(");
       pr_constant(pp, const);
       prStr(pp, ")");
       PP.emit(pp, PP.END);
       prBlank(pp);
       pr_exp(pp, exp);
       prStr(pp, ")");
       PP.emit(pp, PP.END))

    and pr_exp(pp, CPS.EXP r) = pr_exp'(pp, !r)

    fun pr_export pp (CPS.DEF{name,uses,...}) =
      if !uses < 0 then (prBlank(pp); pr_longid(pp, name)) else ()

    fun pr_define pp (CPS.DEF{name, uses, v_tvs, v_fc, v_sc, body, ...}) =
      if !uses = 0 then ()
      else
	(prNewLine(pp);
	 prStr(pp, "(define (");
	 PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=0});
	 pr_longid(pp, name);
	 List.app (pr_space_var pp) v_tvs;
	 pr_space_var pp v_fc;
	 pr_space_var pp v_sc;
	 prStr(pp, ")");
	 PP.emit(pp, PP.END);
	 prNewLine(pp);
	 PP.emit(pp, PP.BEGIN{breakType=PP.CONSISTENT, offset=2});
	 prStr(pp, "  ");
	 pr_exp(pp, body);
	 prStr(pp, ")");
	 PP.emit(pp, PP.END);
	 prNewLine(pp))

    fun pr_value pp (name,lit) =
      (prNewLine(pp);
       prStr(pp, "(define ");
       pr_longid(pp, name);
       prStr(pp, " `");
       pr_literal(pp, lit);
       prStr(pp, ")");
       prNewLine(pp))

    fun pr_blank_value_name pp (name,_) = (prBlank(pp); pr_longid(pp,name))

    fun pr_module(pp, CPS.MODULE{name, xmods, values, defines, ...}) =
      (prStr(pp, "(MODULE "); prStr(pp, name); prStr(pp, ")"); prNewLine(pp);
       PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=6});
       prStr(pp, "(WITH");
       List.app (fn xmod => (prBlank pp; prStr(pp, xmod))) xmods;
       prStr(pp, ")");
       PP.emit(pp, PP.END);
       prNewLine(pp);
       PP.emit(pp, PP.BEGIN{breakType=PP.INCONSISTENT, offset=9});
       prStr(pp, "(EXPORTS");
       List.app (pr_blank_value_name pp) values;
       List.app (pr_export pp) defines;
       prStr(pp, ")");
       PP.emit(pp, PP.END);
       prNewLine(pp);
       List.app (pr_value pp) values;
       List.app (pr_define pp) defines)

    fun printModule(os, module) =
      let val pp = PP.init(os, 80)
      in
	PP.emit(pp, PP.BEGIN{offset=0, breakType=PP.CONSISTENT});
	pr_module(pp, module);
	PP.emit(pp, PP.END);
	PP.close(pp)
      end

  end (* functor CPSPrintFn *)
