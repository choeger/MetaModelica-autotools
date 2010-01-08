(* cpstosml/cpstosml.sml
 * Simple hack to output CPS as executable SML code.
 * Known limitations of this implementation:
 *	* No first-class relations.
 *	* A module's relations are placed in a big letrec. This almost works,
 *	  except that it loses some polymorphism in SML. The dependency graph
 *	  should be output as a sequence of strongly connected components.
 *	* Pattern-matching on real and string literals is not yet implemented.
 *)
functor CPSToSMLFn(structure CPS : CPS) : CPSTOSML =
  struct

    structure CPS = CPS
    structure ConRep = CPS.ConRep

    exception CPSToSML
    fun wrong msg =
      (TextIO.output(TextIO.stdErr, "CPSToSML.");
       TextIO.output(TextIO.stdErr, msg);
       TextIO.output(TextIO.stdErr, "\n");
       raise CPSToSML)

    val output = TextIO.output

    val sml_kwds = [
		    "abstraction", "abstype", "and", "andalso", "as", "case",
		    "do", "datatype", "else", "end", "eqtype", "exception",
		    "fn", "fun", "functor", "handle", "if", "in", "include",
		    "infix", "infixr", "let", "local", "nonfix", "of", "op",
		    "open", "orelse", "raise", "rec", "sharing", "sig",
		    "signature", "struct", "structure", "then", "type", "val",
		    "with", "withtype", "while"
		    ]
    fun is_sml_kwd id =
      let fun member([]) = false
	    | member(k::ks) = (id = k) orelse member ks
      in
	member sml_kwds
      end

    fun prIdent(os, ident) =
      (output(os, ident); if is_sml_kwd ident then output(os, "'") else ())

    fun prLocalLongId(os, ConRep.LONGID{name,...}) = prIdent(os, CPS.identName name)

    fun prLongId(os, ConRep.LONGID{module,name}) =
      (prIdent(os, case module of SOME(module) => (CPS.identName module)^"." | _ => ""); prIdent(os, CPS.identName name))

    fun prInt(os, i) = output(os, Int.toString i)
    fun prReal(os, r) = output(os, Real.toString r)
    fun prString(os, s) =
      (output(os, "\""); output(os, String.toString s); output(os, "\""))

    fun prConst(os, CPS.INTcon i) = (output(os, "rml.INT "); prInt(os, i))
      | prConst(os, CPS.HDRcon{len,con}) = prInt(os, len*256+con*4+0)
      | prConst(os, CPS.REALcon r) = (output(os, "rml.REAL "); prReal(os, r))
      | prConst(os, CPS.STRINGcon s) = (output(os, "rml.STRING "); prString(os, s))

    fun unopToStr(CPS.FETCH i) = "rml.prim_fetch"^(Int.toString i)
      | unopToStr(CPS.BOOL_NOT) = "rml.prim_bool_not"
      | unopToStr(CPS.INT_NEG) = "rml.prim_int_neg"
      | unopToStr(CPS.INT_ABS) = "rml.prim_int_abs"

    fun prUnOp(os, unop) = output(os, unopToStr unop)

    fun binopToStr(CPS.EQUAL) = "rml.prim_equal"
      | binopToStr(CPS.BOOL_AND) = "rml.prim_bool_and"
      | binopToStr(CPS.BOOL_OR) = "rml.prim_bool_or"
      | binopToStr(CPS.INT_ADD) = "rml.prim_int_add"
      | binopToStr(CPS.INT_SUB) = "rml.prim_int_sub"
      | binopToStr(CPS.INT_MUL) = "rml.prim_int_mul"
      | binopToStr(CPS.INT_DIV) = "rml.prim_int_div"
      | binopToStr(CPS.INT_MOD) = "rml.prim_int_mod"
      | binopToStr(CPS.INT_MAX) = "rml.prim_int_max"
      | binopToStr(CPS.INT_MIN) = "rml.prim_int_min"
      | binopToStr(CPS.INT_LT) = "rml.prim_int_lt"
      | binopToStr(CPS.INT_LE) = "rml.prim_int_le"
      | binopToStr(CPS.INT_EQ) = "rml.prim_int_eq"
      | binopToStr(CPS.INT_NE) = "rml.prim_int_ne"
      | binopToStr(CPS.INT_GE) = "rml.prim_int_ge"
      | binopToStr(CPS.INT_GT) = "rml.prim_int_gt"

    fun prBinOp(os, binop) = output(os, binopToStr binop)

    fun prVar(os, CPS.VAR{tag,...}) = (output(os, "x"); output(os, Int.toString tag))

    fun prProc(os, CPS.EXTERN_REL(lid,_)) = prLongId(os, lid)
      | prProc(os, CPS.LOCAL_REL(CPS.DEF{name,...})) = prLocalLongId(os, name)

    fun prStruct(os, prCommaX, tag, xs) =
      let fun loop([]) = output(os, ")")
	    | loop(x::xs) = (prCommaX(os, x); loop xs)
      in
	output(os, "rml.TUPLE");
	output(os, Int.toString(1 + length xs));
	output(os, "(rml.INT ");
	output(os, Int.toString tag);
	loop xs
      end

    fun prLit(os, CPS.CONSTlit c) = prConst(os, c)
      | prLit(os, CPS.STRUCTlit(tag, lits,_)) = prStruct(os, prCommaLit, tag, lits)
      | prLit(os, CPS.EXTERNlit lid) = prLongId(os, lid)
      | prLit(os, CPS.PROClit p) = wrong "prLit: CPS.PROClit"

    and prCommaLit(os, lit) = (output(os, ","); prLit(os, lit))

    (*fun prFormal(os, v) = (prVar(os, v); output(os, ":rml.Val"))*)
    val prFormal = prVar

    fun prFormals(os, []) = output(os, "()")
      | prFormals(os, v::vs) =
	  let fun loop([]) = output(os, ")")
		| loop(v::vs) = (output(os, ","); prFormal(os, v); loop vs)
	  in
	    output(os, "(");
	    prFormal(os, v);
	    loop vs
	  end

    fun prKind(os, CPS.FClk) = output(os, "rml.FC(fn() => ")
      | prKind(os, CPS.SClk{v_tvs}) =
	  (output(os, "rml.SC");
	   output(os, Int.toString(length v_tvs));
	   output(os, "(fn");
	   prFormals(os, v_tvs);
	   output(os, " => "))

    fun prTriv(os, t) =
      case CPS.getTE t
	of CPS.VARte v => prVar(os, v)
	 | CPS.LAMte{kind,body,...} =>
	    (prKind(os, kind); prExp(os, body); output(os, ")"))
	 | CPS.QUOTEte l => prLit(os, l)

    and prCommaTriv(os, t) = (output(os, ","); prTriv(os, t))

    and prTrivList(os, []) = output(os, "()")
      | prTrivList(os, t::ts) =
	  let fun loop([]) = output(os, ")")
		| loop(t::ts) = (prCommaTriv(os, t); loop ts)
	  in
	    output(os, "(");
	    prTriv(os, t);
	    loop ts
	  end

    and prPrim(os, CPS.MARKERp) = output(os, "rml.prim_marker()")
      | prPrim(os, CPS.MKSTRUCTp(tag,ts)) = prStruct(os, prCommaTriv, tag, ts)
      | prPrim(os, CPS.UNARYp(unop,t)) =
	  (prUnOp(os, unop); output(os, " "); prTriv(os, t))
      | prPrim(os, CPS.BINARYp(binop,t1,t2)) =
	  (prBinOp(os, binop); output(os, "("); prTriv(os, t1);
	   output(os, ","); prTriv(os, t2); output(os, ")"))

    and prExp(os, e) =
      case CPS.getExp e
	of CPS.AppFCe{fc=t,name=name,pos=pos} =>
	    (output(os, "rml.prim_app_fc ");
	     prTriv(os, t))
	 | CPS.AppSCe{sc,args,name,pos} =>
	    (output(os, "rml.prim_app_sc");
	     output(os, Int.toString(length args));
	     prTrivList(os, sc::args))
	 | CPS.AppPVe{pv,args,fc,sc,name,pos} =>
	    (case CPS.getTE pv
	       of CPS.QUOTEte(CPS.PROClit proc)	=>
		    (prProc(os, proc); prTrivList(os, sc::fc::args))
		| _ => wrong "prExp: AppPV: only PROClit supported")
	 | CPS.LetLABe(CPS.LAB{tag,bvars,body,...}, e) =>
	    (output(os, "let val lab");
	     output(os, Int.toString tag);
	     output(os, " = fn");
	     prFormals(os, bvars);
	     output(os, " => ");
	     prExp(os, body);
	     output(os, " in ");
	     prExp(os, e);
	     output(os, " end"))
	 | CPS.AppLABe(CPS.LAB{tag,...},args) =>
	    (output(os, "lab");
	     output(os, Int.toString tag);
	     prTrivList(os, args))
	 | CPS.RESTOREe(t,e) =>
	    (output(os, "(rml.prim_restore ");
	     prTriv(os, t);
	     output(os, "; ");
	     prExp(os, e);
	     output(os, ")"))
	 | CPS.LETe(v,t,e) =>
	    (output(os, "let val ");
	     prFormal(os, v);
	     output(os, " = ");
	     prTriv(os, t);
	     output(os, " in ");
	     prExp(os, e);
	     output(os, " end"))
	 | CPS.PRIMe(v,p,e) =>
	    (output(os, "let val ");
	     prFormal(os, v);
	     output(os, " = ");
	     prPrim(os, p);
	     output(os, " in ");
	     prExp(os, e);
	     output(os, " end"))
	 | CPS.SWITCHe(_,[],NONE) => wrong "prExp: SWITCH(_,[],NONE)"
	 | CPS.SWITCHe(_,[],SOME e) => prExp(os, e)
	 | CPS.SWITCHe(t, (c as (CPS.INTcon _,_))::cs, default) =>
	    let fun loop([]) = ()
		  | loop(c::cs) = (output(os, " | "); prIntCase(os, c); loop cs)
	    in
	      output(os, "(case rml.prim_get_int ");
	      prTriv(os, t);
	      output(os, " of ");
	      prIntCase(os, c);
	      loop cs;
	      prIntDefault(os, default);
	      output(os, ")")
	    end
	 | CPS.SWITCHe(_, (CPS.HDRcon _,_)::_, _) => wrong "prExp: SWITCH HDR"
	 | CPS.SWITCHe(_, (CPS.REALcon _,_)::_, _) => wrong "prExp: SWITCH REAL"
	 | CPS.SWITCHe(_, (CPS.STRINGcon _,_)::_, _) => wrong "prExp: SWITCH STRING"

    and prIntCase(os, (CPS.INTcon i,e)) =
	  (prInt(os, i); output(os, " => "); prExp(os, e))
      | prIntCase(_, _) = wrong "prIntCase"

    and prIntDefault(os, NONE) = ()
      | prIntDefault(os, SOME e) = (output(os, " | _ => "); prExp(os, e))

    fun prValDef os (lid, lit) =
      (output(os, "    val "); prLocalLongId(os, lid); output(os, " = ");
       prLit(os, lit); output(os, "\n"))
      
    fun prRelDef(os, CPS.DEF{uses,name,v_tvs,v_fc,v_sc,body,...}) =
      (prLocalLongId(os, name);
       prFormals(os, v_sc::v_fc::v_tvs);
       output(os, " = ");
       prExp(os, body);
       output(os, "\n"))

    fun prRelDefs(_, []) = ()
      | prRelDefs(os, def::defs) =
	  let fun loop([]) = ()
		| loop(def::defs) =
		    (output(os, "    and "); prRelDef(os, def); loop defs)
	  in
	    output(os, "    fun ");
	    prRelDef(os, def);
	    loop defs
	  end

    fun prImpMod os name =
      (output(os, "\tstructure "); prIdent(os, name); output(os, " : ");
       output(os, name); output(os, "_sig\n"))

    fun prImpShare(os, xmods) =
      let fun emit(xmod, xmods) =
	    (output(os, " = "); prIdent(os, xmod); output(os, ".rml");
	     case xmods
	       of [] => output(os, "\n")
		| (xmod::xmods) => emit(xmod, xmods))
	  fun look([]) = ()
	    | look(xmod::xmods) =
		if xmod = "rml" then look xmods
		else (output(os, "\tsharing rml"); emit(xmod, xmods))
      in
	look xmods
      end

    fun prCtorDef os (lid,ctor) =
      let fun doit(CPS.ConRep.INT i) = (output(os, "rml.INT "); prInt(os, i))
	    | doit(CPS.ConRep.TRANSPARENT) = output(os, "rml.ctorI")
	    | doit(CPS.ConRep.BOX{arity,tag,...}) =
		(output(os, "rml.ctor_box");
		 output(os, Int.toString arity);
		 output(os, " ");
		 output(os, Int.toString tag))
      in
	output(os, "    val ");
	prLocalLongId(os, lid);
	output(os, " = ");
	doit ctor;
	output(os, "\n")
      end

    fun emitModule(os, ((prefix, ext), CPS.MODULE{name,ctors,xmods,values,defines,source})) =
      (output(os, "(* module "); prIdent(os, name); output(os, " *)\n");
       output(os, "functor "); prIdent(os, name); output(os, "_fun(\n");
       app (prImpMod os) xmods;
       prImpShare(os, xmods);
       output(os, "\t) : "); prIdent(os, name); output(os, "_sig =\n  struct\n");
       output(os, "    structure rml = rml\n");
       app (prValDef os) values;
       prRelDefs(os, defines);
       app (prCtorDef os) ctors;
       output(os, "  end (* functor "); prIdent(os, name); output(os, "_fun *)\n"))

    fun prValSig os (lid, _) =
      (output(os, "    val "); prLocalLongId(os, lid); output(os, " : rml.Val\n"))

    fun prRelSig os (CPS.DEF{name,uses,v_tvs,...}) =
      let fun loop([]) = output(os, " -> unit\n")
	    | loop(_::vs) = (output(os, " * rml.Val"); loop vs)
      in
	if !uses < 0 then
	  (output(os, "    val ");
	   prLocalLongId(os, name);
	   output(os, " : rml.Val * rml.Val");
	   loop v_tvs)
	else ()
      end

    fun prCtorSig os (lid,ctor) =
      let fun doit(CPS.ConRep.INT _) = output(os, "rml.Val")
	    | doit(CPS.ConRep.TRANSPARENT) = output(os, "rml.Val -> rml.Val")
	    | doit(CPS.ConRep.BOX{arity,...}) =
		let fun loop 0 = output(os, " -> rml.Val")
		      | loop a = (output(os, " * rml.Val"); loop(a-1))
		in
		  output(os, "rml.Val");
		  if arity > 0 then loop(arity - 1) else ()
		end
      in
	output(os, "    val ");
	prLocalLongId(os, lid);
	output(os, " : ");
	doit ctor;
	output(os, "\n")
      end

    fun emitInterface(os, CPS.MODULE{name,ctors,xmods,values,defines,source}) =
      (output(os, "(* interface "); prIdent(os, name); output(os, " *)\n");
       output(os, "signature "); prIdent(os, name); output(os, "_sig =\n  sig\n");
       output(os, "    structure rml : rml_sig\n");
       app (prValSig os) values;
       app (prRelSig os) defines;
       app (prCtorSig os) ctors;
       output(os, "  end (* signature "); prIdent(os, name); output(os, "_sig *)\n"))

  end (* functor CPSToSMLFn *)
