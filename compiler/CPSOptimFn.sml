(* cps/cps_optim.sml *)

functor CPSOptimFn(structure Util : UTIL
		   structure CPS : CPS
		   structure CPSPrint : CPS_PRINT
		   structure CPSUsages : CPS_USAGES
		   sharing CPS = CPSPrint.CPS = CPSUsages.CPS
		     ) : CPS_OPTIM =
  struct

    structure CPS = CPS

    fun bug s = Util.bug("CPSOptim."^s)

    val dirtyP = ref false
    infix <--
    fun op <-- (rx,x) = (rx := x; dirtyP := true)

    (* not_free_in : CPS.Var list -> CPS.TrivExp -> bool
     * Recursively checks that no variable in the variable list occurs
     * free in the given trivial expression.
     * Since all bound variables are assumed unique, this simplifies to
     * just checking that none of the variables occur at all in the trivexp.
     *)

    fun same_var(CPS.VAR{tag=tag1,...}) =	(* should be defined in CPS *)
      fn(CPS.VAR{tag=tag2,...}) => tag1=tag2

    fun not_free_in vars =
      let fun notin_exp(CPS.EXP(ref e)) = notin_exp' e
	  and notin_exp'(CPS.AppFCe{fc=t_fc,...}) = notin_te t_fc
	    | notin_exp'(CPS.AppSCe{sc,args,...}) =
		notin_te sc andalso List.all notin_te args
	    | notin_exp'(CPS.AppPVe{pv,args,fc,sc,...}) =
		notin_te pv andalso notin_te fc andalso notin_te sc
		andalso List.all notin_te args
	    | notin_exp'(CPS.LetLABe(CPS.LAB{body,...}, exp)) =
		notin_exp body andalso notin_exp exp
	    | notin_exp'(CPS.AppLABe(CPS.LAB{body,...}, t_star)) =
		List.all notin_te t_star andalso notin_exp body
	    | notin_exp'(CPS.RESTOREe(t,e)) = notin_te t andalso notin_exp e
	    | notin_exp'(CPS.LETe(_,t,e)) = notin_te t andalso notin_exp e
	    | notin_exp'(CPS.PRIMe(_,p,e)) = notin_prim p andalso notin_exp e
	    | notin_exp'(CPS.SWITCHe(t,cases,default)) =
		notin_te t andalso notinDefault default
		andalso List.all notin_case cases
	  and notinDefault(NONE) = true
	    | notinDefault(SOME e) = notin_exp e
	  and notin_case(_,e) = notin_exp e
	  and notin_te te = notin_te'(CPS.getTE te)
	  and notin_te'(CPS.VARte var) = not(List.exists (same_var var) vars)
	    | notin_te'(CPS.LAMte{body,...}) = notin_exp body
	    | notin_te'(CPS.QUOTEte _) = true
	  and notin_prim(CPS.MARKERp) = true
	    | notin_prim(CPS.MKSTRUCTp(_,ts)) = List.all notin_te ts
	    | notin_prim(CPS.UNARYp(_,t)) = notin_te t
	    | notin_prim(CPS.BINARYp(_,t1,t2)) = notin_te t1 andalso notin_te t2
      in
				notin_te
      end

    fun same_var_te(var, te) =
    case CPS.getTE te
		of CPS.VARte var'	=> same_var var var'
		 | _			=> false

    fun same_vars_ts([], []) = true
      | same_vars_ts(var::vars, t::ts) =
	  same_var_te(var,t) andalso same_vars_ts(vars, ts)
      | same_vars_ts(_, _) = false

    fun is_not_lambda(CPS.LAMte _) = false
      | is_not_lambda(_) = true

    fun is_quote te =
    case CPS.getTE te
		of CPS.QUOTEte _	=> true
		 | _			=> false

    fun get_quote te =
    case CPS.getTE te
		of CPS.QUOTEte lit	=> lit
		 | _			=> bug "get_quote"

    fun prim_deletable(CPS.BINARYp(CPS.EQUAL,_,_)) = false
      | prim_deletable(_) = true

    fun lit2constmatcher(CPS.CONSTlit c) = (fn c' => CPS.constEqual(c,c'))
      | lit2constmatcher(_) = bug "lit2constmatcher"

    fun find_matching_case(lit, cases, default) =
    let val found = lit2constmatcher lit
			  fun loop([]) =
				(case default
				   of SOME exp => exp	(* beta_sw2 *)
				    | NONE => bug "find_matching_case (PMC broken?)")
			    | loop((const,exp)::cases) =
				if found const then exp	(* beta_sw1 *) else loop cases
      in
				loop cases
      end

    (* Try to simplify UNARY(FETCH off, QUOTE lit).
     * The literal should be a STRUCT, anything else is an error.
     *)
    fun fetch_lit(0, CPS.STRUCTlit(con,lits,_)) =
	  SOME(CPS.CONSTlit(CPS.HDRcon{con=con, len=length lits}))
      | fetch_lit(off, CPS.STRUCTlit(_,lits,_)) = SOME(List.nth(lits, off-1))
      | fetch_lit(_, _) = bug "fetch_lit"

    val qfalse	= CPS.QUOTEte(CPS.CONSTlit(CPS.INTcon 0))
    val qtrue	= CPS.QUOTEte(CPS.CONSTlit(CPS.INTcon 1))

    fun inline_app([],[],e) = e
      | inline_app(v::vs,t::ts,e) = inline_app(vs, ts, CPS.LETe(v,t,CPS.EXP(ref e)))
      | inline_app(_,_,_) = bug "inline_app: arity error"

    fun reduce_te' root =
    case !root
		of CPS.VARte _			=> ()
		 | CPS.QUOTEte _		=> ()
		 | CPS.LAMte{kind,body,...}	=>
			let val _ = reduce_exp body
			val exp = CPS.getExp body
			in
			  case kind
				of CPS.FClk		=>
					(case exp
					   of CPS.AppFCe{fc=t_fc,name=name,pos=pos} => root <-- CPS.getTE t_fc	(* eta_fc *)
					| _		=> ())
				 | CPS.SClk{v_tvs}	=>
					(case exp
					   of CPS.AppSCe{sc,args,name,pos}	=>
						  if not_free_in v_tvs sc andalso same_vars_ts(v_tvs, args)
						  then root <-- CPS.getTE sc		(* eta_sc *)
						  else ()
					| _	=> ())
			end

    and reduce_te te = reduce_te'(CPS.getTE' te)
    and simplify_te te =
      let val root = CPS.getTE' te
      in
				reduce_te' root;
				!root
      end

    and reduce_prim(e0, root0, var, p, e1) =
    case p
		of CPS.MARKERp		=> reduce_exp e1
		 | CPS.MKSTRUCTp(con,ts) =>
				let val _ = List.app reduce_te ts
				in
				  if List.all is_quote ts then	(* beta_mkstruct *)
					let val lit = CPS.STRUCTlit(con, map get_quote ts, CPS.dummyLongIdent)
					in
					  root0 <-- CPS.LETe(var, CPS.mkQUOTEte lit, e1);
					  reduce_exp e0
					end
				  else reduce_exp e1
				end
		 | CPS.UNARYp(unop,te)	=>
			let val _ = reduce_te te
			in
			case unop
			of CPS.FETCH off=>
				(case CPS.getTE te
				   of CPS.QUOTEte lit	=>
					(case fetch_lit(off, lit)
					   of SOME lit'	=>
						(root0 <-- CPS.LETe(var, CPS.mkQUOTEte lit', e1);
						 reduce_exp e0)
					| NONE		=> reduce_exp e1)
				| _			=> reduce_exp e1)
			 | _		=> reduce_exp e1
			end
		 | CPS.BINARYp(_,t1,t2)	=> (reduce_te t1; reduce_te t2; reduce_exp e1)(*XXX*)

    and reduce_exp(e0 as (CPS.EXP root0)) =
    case !root0
		of CPS.AppFCe{fc=t_fc,...}	=>
			(case simplify_te t_fc
			   of CPS.LAMte{kind=CPS.FClk,body=CPS.EXP root1,...} => root0<--(!root1)
			| _		=> ())
		 | CPS.AppSCe{sc,args,name,pos}	=>
			let val _ = List.app reduce_te args
			in
			  case simplify_te sc
				of CPS.LAMte{kind=CPS.SClk{v_tvs},body,...} =>	(* beta_sc *)
					root0 <-- inline_app(v_tvs, args, CPS.getExp body)
				 | _		=> ()
			end
		 | CPS.AppPVe{pv,args,fc,sc,name,pos}	=>
			let val _ = reduce_te sc
			val _ = reduce_te fc
			val _ = List.app reduce_te args
			in
			  case simplify_te pv
				of CPS.QUOTEte(CPS.PROClit proc)	=>
					(case proc
					   of CPS.LOCAL_REL(CPS.DEF{uses as ref 1,v_tvs,v_fc,v_sc,body,...}) =>
						let val e = CPS.LETe(v_fc, fc, CPS.EXP(ref(CPS.LETe(v_sc, sc, body))))
						in
						  root0 <-- inline_app(v_tvs, args, e);   (* beta_proc *)
						  uses := 0
						end
					| CPS.LOCAL_REL _		=> ()
					| CPS.EXTERN_REL(_,NONE)	=> ()
					| CPS.EXTERN_REL(_,SOME inliner)=> (* try beta_inline *)
						(case inliner{args=args,fc=fc,sc=sc}
						   (* Do NOT continue reducing the new expression
							* that the inliner just gave us. This is because
							* the reference counters of bound variables in
							* e1 will be wrong, leading to invalid rewrites.
							*)
						   of SOME e1	=> root0 <-- e1
						| NONE		=> ()))
				 | _	=> ()
			end
		 | CPS.LetLABe(CPS.LAB{uses,body,...}, e1 as (CPS.EXP root1)) =>
			if !uses < 2 then (root0 <-- !root1; reduce_exp e0)
			else (reduce_exp body; reduce_exp e1)
		 | CPS.AppLABe(CPS.LAB{uses,bvars,body,...}, t_star) =>
			if !uses = 1 then 
			  (root0 <-- inline_app(bvars, t_star, CPS.getExp body); reduce_exp e0)
			else List.app reduce_te t_star
		 | CPS.RESTOREe(te, exp)	=>
			let val _ = reduce_exp exp
			val exp' = CPS.getExp exp
			in
			  case exp'
			of CPS.AppFCe _	=> root0 <-- exp'	(* beta_restore *)
			 | _		=> reduce_te te
			end
		 | CPS.LETe(var as CPS.VAR{uses,subst,...},te,e1 as (CPS.EXP root1))=>
			let val te' = simplify_te te
			in
			  if !uses < 2 orelse is_not_lambda te' then (* beta_{alpha,lit,lam} *)
				((case te'
					of CPS.VARte(CPS.VAR{uses=uses',...}) =>
					uses' := !uses' - 1 + !uses
					 | _ => ());
				 subst := SOME te';
				 root0 <-- (!root1);
				 reduce_exp e0)
			  else
				reduce_exp e1
			end
		 | CPS.PRIMe(var as CPS.VAR{uses,...}, p, e1 as (CPS.EXP root1))	=>
				if !uses = 0 andalso prim_deletable p then	(* beta_prim *)
				  (root0 <-- !root1; reduce_exp e0)
				else reduce_prim(e0, root0, var, p, e1)
		 | CPS.SWITCHe(te,cases,default)	=>
			(case simplify_te te
			   of CPS.QUOTEte lit	=>
				let val exp = find_matching_case(lit,cases,default)
				in
				  root0 <-- CPS.getExp exp;
				  reduce_exp e0
				end
			| CPS.LAMte _	=> bug "reduce_exp: SWITCH on LAM"
			| CPS.VARte var	=> (reduce_cases(var,cases); reduceDefault default)
				)

    and reduceDefault(NONE) = ()
      | reduceDefault(SOME exp) = reduce_exp exp

    and reduce_case(_,exp) = reduce_exp exp

    and reduce_cases(CPS.VAR{subst,...}, cases) =	(* beta_sw3 *)
    let val old_subst = !subst	(* should be NONE *)
	  fun reduce_case(const,exp) =
	    (subst := SOME(CPS.QUOTEte(CPS.CONSTlit const)); reduce_exp exp)
      in
				List.app reduce_case cases;
				subst := old_subst
      end

    fun reduce_def(CPS.DEF{uses,body,...}) =
      if !uses = 0 then () else reduce_exp body

    fun optimize(cps_os, module) =
      let val (module as CPS.MODULE{defines,...}) = CPSUsages.update module
      in
			(case cps_os
			   of SOME os	=> (TextIO.output(os, "\nApplying local CPS-optimizations:\n");
						CPSPrint.printModule(os, module))
				| NONE	=> ());
				dirtyP := false;
				List.app reduce_def defines;
				if !dirtyP then optimize(cps_os, module) else module
      end

  end (* functor CPSOptimFn *)
