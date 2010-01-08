(* cps/cps_fvars.sml *)

functor CPSFVarsFn(structure MakeString : MAKESTRING
		   structure Util : UTIL
		   structure CPS : CPS
		     ) : CPS_FVARS =
  struct

    structure CPS = CPS

    datatype VarFrame = FREE of CPS.var list ref
					  | BOUND of CPS.var list

    fun frame_enter(frame, var as CPS.VAR{tag,...}) =
      let fun loop([]) = frame := var::(!frame)
	    | loop(CPS.VAR{tag=tag',...}::vars) = if tag=tag' then () else loop vars
      in
		loop(!frame)
      end

    fun frame_boundP(frame, CPS.VAR{tag,...}) =
      let fun loop([]) = false
	    | loop(CPS.VAR{tag=tag',...}::frame) = (tag=tag') orelse loop frame
      in
		loop frame
      end

    fun stack_enter stack (var as CPS.VAR{tag,...}) =
      let fun loop([]) = Util.warn("CPSFVarsFn.stack_enter: variable "^(MakeString.icvt tag)^" is unbound!")
	    | loop((FREE frame)::stack) = (frame_enter(frame,var); loop stack)
	    | loop((BOUND frame)::stack) =
		if frame_boundP(frame,var) then () else loop stack
      in
		loop stack
      end

    fun scan_te' stack te' =
      case te'
		of CPS.VARte(var)			=> stack_enter stack var
		 | CPS.LAMte{fvars, kind, body, ...}	=>
			let val stack = (FREE fvars)::stack
			val stack = case kind
					  of CPS.FClk			=> stack
					   | CPS.SClk{v_tvs}	=> BOUND v_tvs :: stack
			in
			  fvars := [];
			  scan_exp stack body
			end
		 | CPS.QUOTEte(_) 	=> ()

    and scan_te stack te = scan_te' stack (CPS.getTE te)

    and scan_prim stack prim =
      case prim
		of CPS.MARKERp			=> ()
		 | CPS.MKSTRUCTp(_, te_star)	=> List.app (scan_te stack) te_star
		 | CPS.UNARYp(_, te)		=> scan_te stack te
		 | CPS.BINARYp(_, te1, te2)	=> (scan_te stack te1; scan_te stack te2)

    and scan_exp' stack exp' =
      case exp'
		of CPS.AppFCe{fc=t_fc, name=name, pos=_} => scan_te stack t_fc
		 | CPS.AppSCe{sc, args,...} =>
			(scan_te stack sc; List.app (scan_te stack) args)
		 | CPS.AppPVe{pv, args, fc, sc, name, pos} =>
			(scan_te stack pv; List.app (scan_te stack) args;
			 scan_te stack fc; scan_te stack sc)
		 | CPS.LetLABe(CPS.LAB{fvars, bvars, body, ...}, exp) =>
			(fvars := [];
			 scan_exp ((BOUND bvars)::(FREE fvars)::stack) body;
			 scan_exp stack exp)
		 | CPS.AppLABe(CPS.LAB{fvars,...}, t_star) =>
			(List.app (scan_te stack) t_star;
			 List.app (stack_enter stack) (!fvars))
		 | CPS.RESTOREe(te, exp) => (scan_te stack te; scan_exp stack exp)
		 | CPS.LETe(var, te, exp) =>
			(scan_te stack te; scan_exp ((BOUND[var])::stack) exp)
			 | CPS.PRIMe(var, prim, exp) =>
			(scan_prim stack prim; scan_exp ((BOUND[var])::stack) exp)
		 | CPS.SWITCHe(te, cases, default)	=>
			(scan_te stack te; List.app (scan_case stack) cases;
			 scanDefault stack default)

    and scanDefault stack (SOME exp) = scan_exp stack exp
      | scanDefault _ NONE = ()

    and scan_case stack (_,exp) = scan_exp stack exp

    and scan_exp stack (CPS.EXP r) = scan_exp' stack (!r)

    fun scan_def(CPS.DEF{uses,v_tvs,v_fc,v_sc,body,...}) =
      if !uses = 0 
      then () 
      else scan_exp [BOUND(v_fc::v_sc::v_tvs)] body

    fun update(CPS.MODULE{defines,...}) = List.app scan_def defines

  end (* functor CPSFVarsFn *)
