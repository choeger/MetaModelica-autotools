(* absyn/absyn_persist.sml *)
functor AbsynPersistFn(structure MakeString : MAKESTRING
		     structure Util : UTIL
		     structure Absyn : ABSYN
		     structure Cache : CACHE
		     structure PERSISTENTParse : PERSISTENT_PARSE where type repository = Cache.repository
		     structure Source : SOURCE
		     structure Control: CONTROL
		     sharing type Absyn.Source.source = Source.source
		     sharing type Absyn.program = PERSISTENTParse.Absyn.program
		     ) : ABSYN_PERSIST =
  struct

    structure Absyn = Absyn
    type repository = Cache.repository
    
    fun bug s = Util.bug("AbsynPersistFn."^s)

    fun prStr(os, s) = TextIO.output(os,s)
    
    fun prC(os) = TextIO.output(os,",")
    fun prSC(os) = TextIO.output(os,";")
    fun prHD(os,str) = TextIO.output(os,str)
    fun prPL(os) = prStr(os, "(");
    fun prPR(os) = prStr(os, ")");
    
    fun print_scon(os, s) = prStr(os, MakeString.scvt s)

	fun prInfo(os, Absyn.INFO(~1,~1)) = ()
	|   prInfo(os, Absyn.INFO(sp,ep)) =
		prStr(os, Int.toString(sp)^","^Int.toString(ep-sp))

	fun print_lit(os, lit as Absyn.CCONlit(c, inf)) = 
	     (prStr(os,"("); prStr(os, Absyn.litString lit); prSC(os); prInfo(os,inf); prPR(os))
	  | print_lit(os, lit as Absyn.ICONlit(i, inf)) = 
	     (prStr(os,"("); prStr(os, Absyn.litString lit); prSC(os); prInfo(os,inf); prPR(os))
	  | print_lit(os, lit as Absyn.RCONlit(r, inf)) =
	     (prStr(os,"("); prStr(os, Absyn.litString lit); prSC(os); prInfo(os,inf); prPR(os))
	  | print_lit(os, lit as Absyn.SCONlit(s, inf)) = 
	     (prStr(os,"("); prStr(os, Absyn.litString lit); prSC(os); prInfo(os,inf); prPR(os))
    
    fun print_sequence(os, b4, between, after, foo_star, print_foo) =
      let fun loop([]) = ()
	    | loop(foo::foo_star) =
		(prStr(os, between); print_foo(os, foo); loop foo_star)
      in
	prStr(os, b4);
	(case foo_star
	   of []		=> ()
	    | (foo::foo_star)	=> (print_foo(os, foo); loop foo_star));
        prStr(os, after)
      end

    fun print_parens_comma(os, foo_star, print_foo) =
      print_sequence(os, "", ",", "", foo_star, print_foo)

    fun print_list(os, [], _) = ()
      | print_list(os, foo_star, print_foo) =
	  print_parens_comma(os, foo_star, print_foo)

    fun print_ident(os, Absyn.IDENT(id, info as Absyn.INFO(sp, ep))) = 
    if (ep-sp = String.size id)
    then (prPL(os); prStr(os, id); prSC(os); prStr(os, Int.toString(sp)); prPR(os)) 
    else (prPL(os); prStr(os, id); prSC(os); prInfo(os, info); prPR(os)) 

    fun print_longid(os, Absyn.LONGID(NONE, ident as Absyn.IDENT(id, info), infoLong)) =
	      (prHD(os,"$L");print_ident(os, ident))
    |   print_longid(os, Absyn.LONGID(SOME(ident'), ident as Absyn.IDENT(id, info), infoLong)) =      
			  (prHD(os,"$L{"); print_ident(os, ident'); 
			   prSC(os); print_ident(os, ident); prSC(os); prInfo(os, infoLong); prStr(os, "}"))

    fun print_tyvar(os, Absyn.IDENT(tyvar,info as Absyn.INFO(sp, ep))) = 
    if (ep-sp = (String.size tyvar)-1)
    then (prPL(os); prStr(os, "'"^tyvar); prSC(os); prStr(os, Int.toString(sp)); prPR(os)) 
    else print_ident(os, Absyn.IDENT("'"^tyvar,info))
    
    fun print_ty(os, Absyn.VARty(tyvar, info)) = 
		(prHD(os,"$Vt"); print_tyvar(os, tyvar))
      | print_ty(os, Absyn.CONSty(tyseq, longtycon, info)) =
	     ( prHD(os, "$Ct(");
	     	 if (List.length tyseq = 0)
	     	 then ()
	     	 else (print_list(os, tyseq, print_ty); prSC(os)); 
	       print_longid(os, longtycon);
	       prSC(os);
	       prInfo(os,info);
	       prPR(os))
      | print_ty(os, Absyn.TUPLEty(tyseq, info)) = 
		  (prHD(os, "$Tt(");
		   prPL(os);
		   print_tuple_ty(os, tyseq);
		   prPR(os);
		   prSC(os);
		   prInfo(os,info);
		   prPR(os))
      | print_ty(os, Absyn.RELty(domtys, codtys, info)) =
		  (prHD(os, "$Rt(");       
		   print_relty(os, domtys, codtys);
		   prSC(os);
	       prInfo(os,info);
	       prPR(os))		   
      | print_ty(os, Absyn.NAMEDty(id, ty, info)) = 
         ( prHD(os, "$Nt(");       
		   print_ident(os, id);
		   prSC(os); 
		   print_ty(os, ty); 
		   prSC(os);
	       prInfo(os,info);
	       prPR(os))

    and print_seqty(os, tyseq) = print_parens_comma(os, tyseq, print_ty)

    and print_tuple_ty(os, tyseq) =
      print_sequence(os, "", ",\n ", "", tyseq, print_ty_tuple)

    and print_ty_tuple(os, Absyn.RELty(domtys, codtys, info)) =
	  ( prHD(os, "$Rt(");       
		print_relty(os, domtys, codtys);
		prSC(os);
	    prInfo(os,info);
	    prPR(os))
      | print_ty_tuple(os, Absyn.TUPLEty(tyseq, info)) =
	  ( prHD(os, "$Tt(");
	    prPL(os);
		print_tuple_ty(os, tyseq);
		prPR(os);
		prSC(os);
		prInfo(os,info);
		prPR(os))
      | print_ty_tuple(os, ty) = print_ty(os, ty)

    and print_relty(os, domtys, codtys) =
      (prPL(os);
       print_seqty(os, domtys); 
       prPR(os);
       prSC(os); 
       prPL(os);
       print_seqty(os, codtys);
       prPR(os))

    fun print_ty_opt(os, SOME ty) = (prHD(os, "$S("); print_ty(os, ty); prPR(os))
      | print_ty_opt(os, NONE) = (prHD(os, "$N"))

    fun print_ctor_opt(os, NONE) = (prHD(os, "$N"))
      | print_ctor_opt(os, SOME longctor) = 
		(  prHD(os, "$S("); 
           print_longid(os, longctor);
           prPR(os))

    fun print_pat(os, Absyn.WILDpat info) = 
		(prHD(os,"$Wp("); prInfo(os, info); prPR(os))
      | print_pat(os, Absyn.LITpat(lit, info)) = 
		(prHD(os,"$Lp("); print_lit(os, lit); prSC(os); prInfo(os, info); prPR(os))
      | print_pat(os, Absyn.CONpat(longcon, info)) = 
		(prHD(os,"$Cp("); print_longid(os, longcon); prSC(os); prInfo(os, info); prPR(os))
      | print_pat(os, Absyn.STRUCTpat(ctor, pat_star, ref pat_stars, info)) =
	    (prHD(os,"$Sp("); 
	     print_ctor_opt(os, ctor);
	     prSC(os);
	     print_parens_comma(os, pat_star, print_pat);
	     prSC(os);
	     prHD(os,"$R(");
	     print_parens_comma(os, pat_stars, print_pat);
	     prPR(os);	     
	     prSC(os); 
	     prInfo(os, info); prPR(os))
      | print_pat(os, Absyn.BINDpat(var, pat, info)) =
	    (prHD(os,"$Bp("); 
	     print_ident(os, var); 
	     prSC(os); 
	     print_pat(os, pat); 
	     prSC(os); 
	     prInfo(os, info); prPR(os))
      | print_pat(os, Absyn.IDENTpat(id, ref pat, info)) = 
		(prHD(os,"$Ip("); 
		 print_ident(os, id); 
		 prSC(os);
		 prHD(os, "$R(");
		 print_pat(os, pat);
		 prPR(os);
	     prSC(os); 
		 prInfo(os, info); 
		 prPR(os))
      | print_pat(os, Absyn.NAMEDpat(id, pat, info)) = 
        (prHD(os,"$Np(");
         print_ident(os, id); 
         prSC(os);
         print_pat(os, pat);
	     prSC(os); 
         prInfo(os, info); 
         prPR(os))

    fun print_exp(os, Absyn.LITexp(lit, info)) = 
		(prHD(os,"$Le("); 
		 print_lit(os, lit); 
	     prSC(os); 
		 prInfo(os, info); 
		 prPR(os))
      | print_exp(os, Absyn.CONexp(longid, info)) = 
		(prHD(os,"$Ce("); 
		 print_longid(os, longid); 
	     prSC(os); 
		 prInfo(os, info); 
		 prPR(os))
      | print_exp(os, Absyn.VARexp(longid, info)) = 
		(prHD(os,"$Ve("); 
		 print_longid(os, longid); 
	     prSC(os); 
		 prInfo(os, info); 
		 prPR(os))
      | print_exp(os, Absyn.STRUCTexp(ctor, exp_star, info)) =
	    (prHD(os,"$Se(");
	     print_ctor_opt(os, ctor); 
	     prSC(os);
	     print_parens_comma(os, exp_star, print_exp); 
	     prSC(os);
	     prInfo(os, info); 
	     prPR(os))
      | print_exp(os, Absyn.IDENTexp(longid, ref exp, info)) = 
		(prHD(os,"$Ie("); 
		 print_longid(os, longid); 
		 prSC(os);
		 prHD(os, "$R(");
		 print_exp(os, exp);
		 prPR(os);
	     prSC(os); 
		 prInfo(os, info); 
		 prPR(os))

    fun print_goal(os, Absyn.CALLgoal(longid, exp_star, pat_star, ref pat_stars, info)) =
	  (
	   prHD(os,"$Cg(");
	   print_longid(os, longid); 
	   prSC(os); 
	   print_parens_comma(os, exp_star, print_exp);
	   prSC(os); 
	   print_parens_comma(os, pat_star, print_pat); 
	   prSC(os); 
	   prHD(os, "$R(");
	   print_parens_comma(os, pat_stars, print_pat); 
	   prPR(os);
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_goal(os, Absyn.EQUALgoal(var1, exp2, info)) =
	  (
	   prHD(os,"$Eg(");
	   print_ident(os, var1); 
	   prSC(os); 
	   print_exp(os, exp2); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_goal(os, Absyn.LETgoal(pat, exp, info)) =
	  (
	   prHD(os,"$Lg(");
	   print_pat(os, pat);
	   prSC(os); 
	   print_exp(os, exp); 
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_goal(os, Absyn.NOTgoal(g, info)) =
	  (
	   prHD(os,"$Ng(");
	   print_goal(os, g); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_goal(os, Absyn.ANDgoal(g1, g2, info)) =
	  (
	   prHD(os,"$Ag(");
	   print_goal(os, g1); 
	   prSC(os);
	   print_goal(os, g2); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_goal(os, Absyn.CONDgoal(g1, g2, g3, info)) =
	  (
	   prHD(os,"$Ig(");
	   print_goal(os, g1); 
	   prSC(os);
	   print_goal(os, g2); 
	   prSC(os); 
	   print_goal(os, g3); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
	   
    fun print_g_opt(os, NONE) = ()
      | print_g_opt(os, SOME goal) = (prHD(os,"$S("); print_goal(os, goal); prPR(os); prSC(os))

    fun prResult(os, Absyn.RETURN(exps, info)) = 
	  (
	   prHD(os,"$Rr(");		
	   print_parens_comma(os, exps, print_exp);
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | prResult(os, Absyn.FAIL info) = (prHD(os, "$Fr("); prInfo(os, info); prPR(os))
      
    fun printAttributes(os, Absyn.ATTRIBUTES{public, final, var, param, const, input, output, bidir}) =
    let val x = ref 0
    in
	   if !public then x := 128 else ();
	   if !final  then x := !x + 64 else ();
	   if !var    then x := !x + 32 else ();
	   if !param  then x := !x + 16 else ();
	   if !const  then x := !x + 8 else ();
	   if !input  then x := !x + 4 else ();
	   if !output then x := !x + 2 else ();
	   if !bidir  then x := !x + 1 else ();
	   prStr(os, Int.toString(!x))
    end
    
    fun printLocalVar(os, (id, ty, exp,attr)) =
	(
	   prHD(os,"$LV(");
	   print_ident(os, id);
	   prSC(os);
	   case ty of SOME(ty) => (prHD(os,"$S("); print_ty(os, ty); prPR(os)) | NONE => prHD(os,"$N"); 
	   prSC(os);
	   case exp of SOME(exp) => (prHD(os,"$S("); print_exp(os, exp); prPR(os)) | NONE => prHD(os,"$N");
	   prSC(os); 
	   printAttributes(os, attr);
	   prPR(os)
	)

    fun print_clause(os, Absyn.CLAUSE1(g_opt, id, pat_star, result, ref pat_stars, localVars, info)) =
	  (
	   prHD(os,"$C1(");			  
	   print_g_opt(os, g_opt);
	   print_ident(os, id);
	   prSC(os);
	   print_parens_comma(os, pat_star, print_pat);
	   prSC(os);
	   prResult(os, result);
	   prSC(os);
	   prHD(os,"$R(");
	   print_parens_comma(os, pat_stars, print_pat);
	   prPR(os);	     	   
	   prSC(os);
	   prPL(os);       
	   print_list(os, localVars, printLocalVar);
       prPR(os); 	   
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_clause(os, Absyn.CLAUSE2(cl1, cl2, info)) =
	  (
	   prHD(os,"$C2(");  
	   print_clause(os, cl1);
	   prSC(os); 
	   print_clause(os, cl2);
	   prSC(os); 	   
	   prInfo(os, info); 
	   prPR(os))

    fun print_conbind(os, Absyn.CONcb(id, info)) = 
       (
	   prHD(os,"$COcb(");         
       print_ident(os, id); 
	   prSC(os);         
       prInfo(os, info); 
       prPR(os))
      | print_conbind(os, Absyn.CTORcb(id, tyseq, info)) =
	  (
	   prHD(os,"$CTcb(");
	   print_ident(os, id); 
	   prSC(os); 	  
	   print_tuple_ty(os, tyseq); 
	   prSC(os); 	  
	   prInfo(os, info); 
	   prPR(os))

    fun print_conbind_star(os, conbind_star) =
      print_sequence(os, "", ",", "", conbind_star, print_conbind)

    fun print_tyvarseq_tycon(os, tyvarseq, tycon) =
      (print_list(os, tyvarseq, print_tyvar); 
	   prSC(os); 
       print_ident(os, tycon))

    fun print_datbind(os, Absyn.DATBIND(tyvarseq, tycon, conbind_star, info)) =
      (
	   prHD(os,"$DA(");               
       print_tyvarseq_tycon(os, tyvarseq, tycon);
       prSC(os);
       print_conbind_star(os, conbind_star);
	   prSC(os);        
       prInfo(os, info); 
       prPR(os))

    fun print_datbind_star(os, datbind_star) =
      print_sequence(os, "", ",", "", datbind_star, print_datbind)

    fun print_typbind(os, Absyn.TYPBIND(tyvarseq, tycon, ty, info)) =
      (
	   prHD(os,"$TY(");               
       print_tyvarseq_tycon(os, tyvarseq, tycon);
       prSC(os);
       print_ty(os, ty); 
       prSC(os); 
       prInfo(os, info); 
       prPR(os))

    fun print_typbind_star(os, typbind_star) =
      print_sequence(os, "", ", ", "", typbind_star, print_typbind)

    fun print_withtype(os, []) = ()
      | print_withtype(os, typbind_star) = print_typbind_star(os, typbind_star)

	fun prMatchExps(os, NONE) = (prHD(os, "$N"))
	|	prMatchExps(os, SOME(exp, info1, pat, info2)) = 
	(
		prHD(os, "$S(");
		print_exp(os, exp);
		prSC(os);
		prInfo(os, info1);
		prSC(os);
		print_pat(os, pat);
		prSC(os);
		prInfo(os, info2);
		prPR(os)
	)

    fun print_relbind(os, Absyn.RELBIND(ident, ty_opt, clause, localVars, matchExps, info)) =
      (
	   prHD(os,"$RE(");   
       print_ident(os, ident); 
       prSC(os);
       print_ty_opt(os, ty_opt);
       prSC(os); 
       print_clause(os, clause); 
       prSC(os); 
	   prPL(os);       
	   print_list(os, localVars, printLocalVar);
       prPR(os); 	   
	   prSC(os);
	   prMatchExps(os, matchExps);
	   prSC(os);
       prInfo(os, info); 
       prPR(os))

    fun print_relbind_star(os, relbind_star) =
      print_sequence(os, "", ", ", "", relbind_star, print_relbind)

    fun print_spec (os, Absyn.WITHspec(str, ref(interface), info)) =
	  (
	   prHD(os,"$Ws(");
	   print_scon(os, str); 
	   prSC(os); 
	   prHD(os, "$R(");
	   print_interface(os, interface);
	   prPR(os);
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_spec (os, Absyn.ABSTYPEspec(eq, tyvarseq, tycon, info)) =
	  (
	   prHD(os,"$Aa(");         	    
	   prStr(os, if eq then "1" else "0");
	   prSC(os); 	   
	   print_tyvarseq_tycon(os, tyvarseq, tycon);
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_spec (os, Absyn.TYPEspec(typbind_star, info)) =
	  ( 
	   prHD(os,"$Ts("); 
	   print_typbind_star(os, typbind_star);
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_spec (os, Absyn.DATAspec(datbind_star, typbind_star, info)) =
	  (
	   prHD(os,"$Ds(");
	   print_datbind_star(os, datbind_star); 
	   prSC(os);
	   print_withtype(os, typbind_star); 
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_spec (os, Absyn.VALspec(ident, ty, info)) =
	  (
	   prHD(os,"$Vs(");	  
	   print_ident(os, ident); 
	   prSC(os);
	   print_ty(os, ty); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_spec (os, Absyn.RELspec(ident, ty, info)) =
	  (
	   prHD(os,"$Rs(");	  
	   print_ident(os, ident); 
	   prSC(os);
	   print_ty(os, ty); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
	   
    and print_dec (os, Absyn.WITHdec(str, ref(interface), info)) =
	  (
	   prHD(os,"$Wd(");	  
	   print_scon(os, str);
	   prSC(os);
	   prHD(os, "$R(");
	   print_interface(os, interface);
	   prPR(os);
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_dec (os, Absyn.TYPEdec(typbind_star, info)) =
	  (
	   prHD(os,"$Td(");	  
	   print_typbind_star(os, typbind_star); 
	   prSC(os);	  
	   prInfo(os, info); 
	   prPR(os))
      | print_dec (os, Absyn.DATAdec(datbind_star, typbind_star, info)) =
	  (
	   prHD(os,"$Dd(");	  
	   print_datbind_star(os, datbind_star);
	   prSC(os);
	   print_withtype(os, typbind_star);
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
      | print_dec (os, Absyn.VALdec(ident, exp, info)) =
	  (
	   prHD(os,"$Vd(");
	   print_ident(os, ident); 
	   prSC(os);
	   print_exp(os, exp); 
	   prSC(os); 
	   prInfo(os, info); 
	   prPR(os))
      | print_dec (os, Absyn.RELdec(relbind_star, info)) =
	  (
	   prHD(os,"$Rd(");
	   print_relbind_star(os, relbind_star);
	   prSC(os);
	   prInfo(os, info); 
	   prPR(os))
	  
	and print_interface(os, Absyn.INTERFACE({modid,specs,source}, info)) =
      (
	   prHD(os,"$INT(");
	   if (Absyn.identName modid) = ""
	   then
	   (
	     prStr(os,"$DINT")
	   )
	   else
	   (	  
       print_ident(os, modid); 
       prSC(os);
	   prPL(os);       
       print_list(os , specs, print_spec);
       prPR(os); 
       prSC(os); 
       print_source(os, source);
	   prSC(os); 
       prInfo(os, info)
       ); 
       prPR(os))
       
    and print_source(os, source) = 
      (
	   prHD(os,"$SRC(");
	   if ((Absyn.Source.getFileName(source)) = "")
	   then prStr(os,"$DSRC(")
	   else
	   (
	   print_scon(os, Absyn.Source.getFileName(source));
       prSC(os); 
       print_scon(os, Absyn.Source.getCurrentDate());
       prSC(os);
       prPL(os);
       let fun buu(os, x) = prStr(os, Int.toString(x))
       in
         print_list(os, Absyn.Source.getLines(source), buu)
       end;
       prPR(os);
       prSC(os);
       prStr(os, Int.toString(Absyn.Source.getCurrentLine(source)))
       );
       prPR(os))
       
    fun printSerializationInfo(os, source) = 
      (
	   prHD(os,"$SerializationInfo(");
	   if ((Absyn.Source.getFileName(source)) = "")
	   then bug("printSerializationInfo. Serialization problem.")
	   else ();
	   print_scon(os, Absyn.Source.getFileName(source));
       prSC(os); 
       print_scon(os, Absyn.Source.getCurrentDate());
       prSC(os);
       prStr(os, Int.toString(Control.serializationFileVersion));
       prStr(os,")\n");
       prStr(os,"\n\n")       
      )       
       
    fun serializeModule(os, 
		Absyn.MODULE(
			interface as Absyn.INTERFACE({modid,specs,source}, info_interface), 
			dec_star, info_module)) =
      (
       printSerializationInfo(os, source);

	   prHD(os,"$MODULE(");
	   print_interface(os, interface);
	   prSC(os);
       prInfo(os, info_module); 
       prStr(os,")\n\n\n");
       prHD(os,"$DEC(");
       print_list(os, dec_star, print_dec);
       prPR(os)
      )
      
    fun parseModule    (file, repository) = PERSISTENTParse.parseModule(file,    repository)
    fun parseInterface (file, repository) = PERSISTENTParse.parseInterface(file, repository)  

end (* functor AbsynPersistFn *)
