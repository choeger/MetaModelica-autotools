(* absyn/absyn_print.sml *)

functor AbsynPrintFn(structure MakeString : MAKESTRING
		     structure Util : UTIL
		     structure Absyn : ABSYN
		     structure Control: CONTROL) : ABSYN_PRINT =
  struct

    structure Absyn = Absyn

    fun prStr(os, s) = TextIO.output(os,s)

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
      print_sequence(os, "(", ",", ")", foo_star, print_foo)

    fun print_list(os, [], _) = ()
      | print_list(os, foo_star, print_foo) =
	  print_parens_comma(os, foo_star, print_foo)

    fun print_ident(os, id) = prStr(os, Absyn.identName id)

    fun print_longid(os, Absyn.LONGID(modname, ident, _)) =
      ((case modname
	  of SOME ident'	=> (print_ident(os, ident'); prStr(os, "."))
	   | NONE		=> ());
       print_ident(os, ident))

    fun print_lit(os, lit) = prStr(os, Absyn.litString lit)

    fun print_scon(os, s) = prStr(os, MakeString.scvt s)

    fun print_tyvar(os, tyvar) = (prStr(os, "'"); print_ident(os, tyvar))

    fun print_ty(os, Absyn.VARty(tyvar, _)) = print_tyvar(os, tyvar)
      | print_ty(os, Absyn.CONSty(tyseq, longtycon, _)) =
	     (print_list(os, tyseq, print_ty); print_longid(os, longtycon))
      | print_ty(os, Absyn.TUPLEty(tyseq, _)) = print_tuple_ty(os, tyseq)
      | print_ty(os, Absyn.RELty(domtys, codtys, _)) = print_relty(os, domtys, codtys)
      | print_ty(os, Absyn.NAMEDty(id, ty, _)) = 
         (print_ident(os, id); prStr(os, ":("); print_ty(os, ty); prStr(os, ")"))

    and print_seqty(os, tyseq) = print_parens_comma(os, tyseq, print_ty)

    and print_tuple_ty(os, tyseq) =
      print_sequence(os, "", "*", "", tyseq, print_ty_tuple)

    and print_ty_tuple(os, Absyn.RELty(domtys, codtys, _)) =
	  (prStr(os, "("); print_relty(os,domtys,codtys); prStr(os, ")"))
      | print_ty_tuple(os, Absyn.TUPLEty(tyseq, _)) =
	  (prStr(os, "("); print_tuple_ty(os, tyseq); prStr(os, ")"))
      | print_ty_tuple(os, ty) = print_ty(os, ty)

    and print_relty(os, domtys, codtys) =
      (print_seqty(os, domtys); prStr(os, " => "); print_seqty(os, codtys))

    fun print_ty_opt(os, SOME ty) = (prStr(os, ": "); print_ty(os, ty))
      | print_ty_opt(_, NONE) = ()

    fun print_ctor_opt(os, NONE) = ()
      | print_ctor_opt(os, SOME longctor) = print_longid(os, longctor)

    fun print_pat(os, Absyn.WILDpat _) = prStr(os, "_")
      | print_pat(os, Absyn.LITpat(lit, _)) = print_lit(os, lit)
      | print_pat(os, Absyn.CONpat(longcon, _)) = print_longid(os, longcon)
      | print_pat(os, Absyn.STRUCTpat(ctor, pat_star, _, _)) =
	     (print_ctor_opt(os, ctor); print_parens_comma(os, pat_star, print_pat))
      | print_pat(os, Absyn.BINDpat(var, pat, _)) =
	  (print_ident(os, var); prStr(os, " as "); print_pat(os, pat))
      | print_pat(os, Absyn.IDENTpat(id, _, _)) = print_ident(os, id)
      | print_pat(os, Absyn.NAMEDpat(id, pat, _)) = 
         (print_ident(os, id); prStr(os, " = "); print_pat(os, pat))

    fun print_exp(os, Absyn.LITexp(lit, _)) = print_lit(os, lit)
      | print_exp(os, Absyn.CONexp(longid, _)) = print_longid(os, longid)
      | print_exp(os, Absyn.VARexp(longid, _)) = print_longid(os, longid)
      | print_exp(os, Absyn.STRUCTexp(ctor, exp_star, _)) =
	  (print_ctor_opt(os, ctor); print_parens_comma(os, exp_star, print_exp))
      | print_exp(os, Absyn.IDENTexp(longid, _, _)) = print_longid(os, longid)

    fun print_goal(os, Absyn.CALLgoal(longid, exp_star, pat_star, _, _)) =
	  (print_longid(os, longid); print_parens_comma(os, exp_star, print_exp);
	   prStr(os, " => "); print_parens_comma(os, pat_star, print_pat))
      | print_goal(os, Absyn.EQUALgoal(var1, exp2, _)) =
	  (print_ident(os, var1); prStr(os, " = "); print_exp(os, exp2))
      | print_goal(os, Absyn.LETgoal(pat, exp, _)) =
	  (prStr(os, "let "); print_pat(os, pat);
	   prStr(os, " = "); print_exp(os, exp))
      | print_goal(os, Absyn.NOTgoal(g, _)) =
	  (prStr(os, "not "); print_atomic_goal(os, g))
      | print_goal(os, Absyn.ANDgoal(g1, g2, _)) =
	  (print_goal(os, g1); prStr(os, " &\n       "); print_goal(os, g2))
      | print_goal(os, Absyn.CONDgoal(g1, g2, g3, _)) =
	  (prStr(os, " if "); print_goal(os, g1); 
	   prStr(os, " then "); print_goal(os, g2); 
	   prStr(os, " else "); print_goal(os, g3);
	   prStr(os, " &\n       "))

    and print_atomic_goal(os, Absyn.ANDgoal(g1, g2, _)) =
	  (prStr(os, "("); print_goal(os, g1); prStr(os, " & ");
	   print_atomic_goal(os, g2); prStr(os, ")"))
      | print_atomic_goal(os, g) = print_goal(os, g)

    fun print_g_opt(os, NONE) = ()
      | print_g_opt(os, SOME goal) = print_goal(os, goal)

    fun prResult(os, Absyn.RETURN(exps, _)) = print_parens_comma(os, exps, print_exp)
      | prResult(os, Absyn.FAIL _) = prStr(os, "fail")

	fun printLocalVars(os, ([])) = prStr(os, "\n")
	|	printLocalVars(os, ((id, SOME(ty), exp, _)::rest)) =
	(
		prStr(os, "\n       val ");
		print_ident(os, id); prStr(os, ":");
		print_ty(os, ty); prStr(os, " ");
		printLocalVars(os, rest)
	)
	|	printLocalVars(os, ((id, _, _, _)::rest)) = 
	(
		prStr(os, "\n       val ");
		print_ident(os, id); prStr(os, ":");
		prStr(os, "?"); prStr(os, " ");
		printLocalVars(os, rest)
	)	

    fun print_clause(os, Absyn.CLAUSE1(g_opt, id, pat_star, result, _, localVars, _)) =
	  (prStr(os, "\n  rule ");
	   prStr(os, "\n  local"); 
	   printLocalVars(os, localVars);
	   prStr(os, "\t");
	   print_g_opt(os, g_opt);
	   prStr(os, "\n       ----\n       ");
	   print_ident(os, id);
	   print_parens_comma(os, pat_star, print_pat);
	   prStr(os, " => ");
	   prResult(os, result);
	   prStr(os, "\n"))
      | print_clause(os, Absyn.CLAUSE2(cl1, cl2, _)) =
	  (print_clause(os, cl1); print_clause(os, cl2))

    fun print_conbind(os, Absyn.CONcb(id, _)) = print_ident(os, id)
      | print_conbind(os, Absyn.CTORcb(id, tyseq, _)) =
	  (print_ident(os, id); prStr(os, " of "); print_tuple_ty(os, tyseq))

    fun print_conbind_star(os, conbind_star) =
      print_sequence(os, "\n  = ", "\n  | ", "\n", conbind_star, print_conbind)

    fun print_tyvarseq_tycon(os, tyvarseq, tycon) =
      (print_list(os, tyvarseq, print_tyvar); print_ident(os, tycon))

    fun print_datbind(os, Absyn.DATBIND(tyvarseq, tycon, conbind_star, _)) =
      (print_tyvarseq_tycon(os, tyvarseq, tycon);
       prStr(os, " "); print_conbind_star(os, conbind_star))

    fun print_datbind_star(os, datbind_star) =
      print_sequence(os, "datatype ", "\nand ", "", datbind_star, print_datbind)

    fun print_typbind(os, Absyn.TYPBIND(tyvarseq, tycon, ty, _)) =
      (print_tyvarseq_tycon(os, tyvarseq, tycon);
       prStr(os, " = "); print_ty(os, ty); prStr(os, "\n"))

    fun print_typbind_star(os, typbind_star) =
      print_sequence(os, "", "and ", "", typbind_star, print_typbind)

    fun print_withtype(os, []) = ()
      | print_withtype(os, typbind_star) =
	  (prStr(os, "withtype "); print_typbind_star(os, typbind_star))

	fun printMatchExps(os, NONE) = 	prStr(os, "\n\tmatchexp: NONE")
	|	printMatchExps(os, SOME(exp, _, pat, _)) =
	(
		prStr(os, "\n\n       matchexp: ");
		print_pat(os, pat); prStr(os, " = "); print_exp(os, exp); 
		prStr(os, "\n       ")
	)
	
    fun print_relbind(os, Absyn.RELBIND(ident, ty_opt, clause, localVars, exps, _)) =
      (
	   print_ident(os, ident); print_ty_opt(os, ty_opt);
       prStr(os, " = ");
       printMatchExps(os, exps);
	   prStr(os, "\n  local"); 
	   printLocalVars(os, localVars);
	   prStr(os, "       ");
	   print_clause(os, clause); 
       prStr(os, "\nend\n"))

    fun print_relbind_star(os, relbind_star) =
      print_sequence(os, "", "and ", "", relbind_star, print_relbind)

    fun print_spec os (Absyn.WITHspec(str, _, _)) =
	  (prStr(os, "with "); print_scon(os, str); prStr(os, "\n"))
      | print_spec os (Absyn.ABSTYPEspec(eq, tyvarseq, tycon, _)) =
	  (prStr(os, if eq then "eqtype " else "type ");
	   print_tyvarseq_tycon(os, tyvarseq, tycon); prStr(os, "\n"))
      | print_spec os (Absyn.TYPEspec(typbind_star, _)) =
	  (prStr(os, "type "); print_typbind_star(os, typbind_star))
      | print_spec os (Absyn.DATAspec(datbind_star, typbind_star, _)) =
	  (print_datbind_star(os, datbind_star); print_withtype(os, typbind_star))
      | print_spec os (Absyn.VALspec(ident, ty, _)) =
	  (prStr(os, "val "); print_ident(os, ident); prStr(os, ": ");
	   print_ty(os, ty); prStr(os, "\n"))
      | print_spec os (Absyn.RELspec(ident, ty, _)) =
	  (prStr(os, "relation "); print_ident(os, ident); prStr(os, ": ");
	   print_ty(os, ty); prStr(os, "\n"))

    fun print_dec os (Absyn.WITHdec(str, _, _)) =
	  (prStr(os, "with "); print_scon(os, str); prStr(os, "\n"))
      | print_dec os (Absyn.TYPEdec(typbind_star, _)) =
	  (prStr(os, "type "); print_typbind_star(os, typbind_star))
      | print_dec os (Absyn.DATAdec(datbind_star, typbind_star, _)) =
	  (print_datbind_star(os, datbind_star); print_withtype(os, typbind_star))
      | print_dec os (Absyn.VALdec(ident, exp, _)) =
	  (prStr(os, "val "); print_ident(os, ident); prStr(os, " = ");
	   print_exp(os, exp); prStr(os, "\n"))
      | print_dec os (Absyn.RELdec([], _)) = ()
      | print_dec os (Absyn.RELdec(relbind_star, _)) =
	  (prStr(os, "relation "); print_relbind_star(os, relbind_star))

    fun printModule(os, Absyn.MODULE(Absyn.INTERFACE({modid,specs,source}, _), dec_star, _)) =
      (prStr(os,"(*moduleOf["); prStr(os, Absyn.Source.getFileName(source)); prStr(os, "]*)\n");
       prStr(os, "module "); print_ident(os, modid); prStr(os, ":\n");
       List.app (print_spec os) specs;
       prStr(os, "end\n");
       List.app (print_dec os) dec_star)

    fun printInterface(os, Absyn.MODULE(Absyn.INTERFACE({modid,specs,source}, _), dec_star, _)) =
      (prStr(os,"(*interfaceOf["); prStr(os, Absyn.Source.getFileName(source)); prStr(os, "]*)\n");
       prStr(os, "module "); print_ident(os, modid); prStr(os, ":\n");
       List.app (print_spec os) specs;
       prStr(os, "end\n"))

    fun print_spec_with os (Absyn.WITHspec(str, _, _)) = 
		(prStr(os, Control.getFileName(str, Control.INTERFACE_FILE)); prStr(os, " "))
      | print_spec_with os (_) = ()

    fun print_dec_with os (Absyn.WITHdec(str, _, _)) = 
		(prStr(os, Control.getFileName(str, Control.INTERFACE_FILE)); prStr(os, " "))
      | print_dec_with os (_) = ()
       
    fun printDependencies(os, Absyn.MODULE(Absyn.INTERFACE({modid,specs,...}, _), dec_star, _)) =
      (List.app (print_spec_with os) specs;
       List.app (print_dec_with os) dec_star)
       
    fun printGoal(os, goal) = print_goal(os, goal)

  end (* functor AbsynPrintFn *)
