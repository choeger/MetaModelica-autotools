(* fol/fol_unify.sml *)

functor FOLUnifyFn(structure Util : UTIL
                   structure FOL : FOL) : FOL_UNIFY =
  struct

    structure FOL = FOL

    val trail = ref([]: FOL.var option ref list)

    fun unwind() =
      let fun loop([]) = trail := []
	    | loop(r::trail) = (r := NONE; loop trail)
      in
		loop(!trail)
      end

    exception Unify

    fun does_not_unify() = (unwind(); raise Unify)

    fun zipapp(f, xs, ys) =
      let fun loop([], []) = ()
	    | loop(x::xs, y::ys) = (f(x, y); loop(xs, ys))
	    | loop(_, _) = does_not_unify()
      in
		loop(xs, ys)
      end

    fun e_var(var1, var2) =
      let val FOL.VAR({inst=r1,...}, _) = FOL.deref var1
	      and FOL.VAR({inst=r2,...}, _) = FOL.deref var2
      in
		if r1 = r2 then () else does_not_unify()
      end

    fun u_var(var1, var2) =
      let val FOL.VAR({inst=r1,...}, _) = FOL.deref var1
	  and (var2 as FOL.VAR({inst=r2,...}, _)) = FOL.deref var2
      in
		if r1 = r2 then ()
		else (trail := r1 :: !trail; r1 := SOME var2)
      end

    fun e_lit(x, y) = if FOL.litEqual(x,y) then () else does_not_unify()
    fun e_atom(x, y) = if x=y then () else does_not_unify()

	fun exposeNames(FOL.LONGID(SOME(id1), id2, _)) = (SOME(FOL.identName(id1)), FOL.identName(id2))  
	|	exposeNames(FOL.LONGID(NONE, id2, _))      = (NONE, FOL.identName(id2))    
    
    fun exposeLongId(SOME lid) = SOME(exposeNames(lid))
	|	exposeLongId(NONE)	   = NONE    

    fun u_pat'(FOL.WILDpat, FOL.WILDpat) = ()
      | u_pat'(FOL.LITpat lit1, FOL.LITpat lit2) = e_lit(lit1, lit2)
      | u_pat'(FOL.CONpat con1, FOL.CONpat con2) = e_atom(exposeNames(con1), exposeNames(con2))
      | u_pat'(FOL.STRUCTpat(ctor1, patl1), FOL.STRUCTpat(ctor2, patl2)) =
	  (e_atom(exposeLongId ctor1, exposeLongId ctor2); zipapp(u_pat, patl1, patl2))
      | u_pat'(_, _) = does_not_unify()
    and u_pat(FOL.PAT(var1,pat1'), FOL.PAT(var2,pat2')) =
      (u_var(var1, var2); u_pat'(pat1', pat2'))

    fun e_vref(FOL.GVAR(lid1), FOL.GVAR(lid2)) = e_atom(exposeNames(lid1), exposeNames(lid2))
      | e_vref(FOL.BVAR var1, FOL.BVAR var2) = e_var(var1, var2)
      | e_vref(_, _) = does_not_unify()

    fun u_exp(FOL.LITexp lit1, FOL.LITexp lit2) = e_lit(lit1, lit2)
      | u_exp(FOL.CONexp con1, FOL.CONexp con2) = e_atom(exposeNames(con1), exposeNames(con2))
      | u_exp(FOL.VARexp vref1, FOL.VARexp vref2) = e_vref(vref1, vref2)
      | u_exp(FOL.STRUCTexp(ctor1, expl1), FOL.STRUCTexp(ctor2, expl2)) =
	  (e_atom(exposeLongId ctor1, exposeLongId ctor2); zipapp(u_exp, expl1, expl2))
      | u_exp(_, _) = does_not_unify()

    fun u_mrules([], []) = ()
      | u_mrules((var1,pat1)::mrules1, (var2,pat2)::mrules2) =
	  (e_var(var1, var2); u_pat(pat1, pat2); u_mrules(mrules1, mrules2))
      | u_mrules(_, _) = does_not_unify()


	(* adrpo added first 2 lines from this function to unify RML.debug calls
	   even if they don't unify. This makes the debugging faster by allowing
	   more FOL optimizations like this one:
	   * ORELSE(ANDTHEN(c1a,d1b), ANDTHEN(c2a,d2b))	if c1a=c2a
       * ==> ANDTHEN(c1a, ORELSE(d1b,d2b))
	   *)
    fun u_conj(FOL.CALL(vref1 as FOL.GVAR(FOL.LONGID(SOME(FOL.IDENT("RML", _)), debug1, _)), 
						expl1, 
						varl1, inf1), 
			   FOL.CALL(vref2 as FOL.GVAR(FOL.LONGID(SOME(FOL.IDENT("RML", _)), debug2, _)), 
						expl2, 
						varl2, inf2)) =
	   (* before we compare expressions lists we have to take into consideration
	      the "not" goals, if they exists *)
	   if ((String.isPrefix "debug" (FOL.identName debug1)) andalso 
	       (String.isPrefix "debug" (FOL.identName debug2)) andalso 
	       (FOL.identName debug1 = FOL.identName debug2))
	   then 
	     (if (FOL.identName debug1 = "debug")
	      then 
	       (case (List.last expl1, List.last expl2)
	        of (FOL.LITexp(x),FOL.LITexp(y)) =>
	            if (FOL.litEqual(x,y)) 
	            then ()
	            else if FOL.litEqual(x, FOL.SCON("not:")) orelse 
	                    FOL.litEqual(y, FOL.SCON("not:"))
	                 then ()
	                 else does_not_unify()
	       | (_,_) => Util.bug("FOLOptim."^"DEBUG optimization");
	       zipapp(u_exp, 
	        (* take only file, relation, goal and drop linenumbers from 
	           RML.debug(file, sline, scolumn, eline, ecolumn, relation, goal) *) 
			  List.take(expl1, 1) @ List.take(List.drop(expl1, 5), 1),  
			  List.take(expl2, 1) @ List.take(List.drop(expl2, 5), 1)) 
	      (*zipapp(u_var, varl1, varl2);*))
	     else (zipapp(u_exp,expl1,expl2) (*zipapp(u_var,varl1,varl2)*))
	     (*; print "Unifying :"; print debug1; print "="; print debug2; print "\n" *) ) 
	   else
	   (e_vref(vref1,vref2); zipapp(u_exp,expl1,expl2); zipapp(u_var,varl1,varl2))
      |u_conj(FOL.CALL(vref1, expl1, varl1, inf1), FOL.CALL(vref2, expl2, varl2, inf2)) =
	  (e_vref(vref1,vref2); zipapp(u_exp,expl1,expl2); zipapp(u_var,varl1,varl2))
      | u_conj(FOL.MATCH(mrules1, inf1), FOL.MATCH(mrules2, inf2)) = u_mrules(mrules1, mrules2)
      | u_conj(FOL.EQUAL(var1, exp1, inf1), FOL.EQUAL(var2, exp2, inf2)) =
	  (e_var(var1, var2); u_exp(exp1, exp2))
      | u_conj(FOL.BIND(var1, exp1, inf1), FOL.BIND(var2, exp2, inf2)) =
	  (u_var(var1, var2); u_exp(exp1, exp2))
      | u_conj(FOL.NOT(conj1,inf1), FOL.NOT(conj2, inf2)) = u_conj(conj1, conj2)
      | u_conj(FOL.AND(c11, c12, inf1), FOL.AND(c21, c22, inf2)) =
	 (u_conj(c11, c21); u_conj(c12, c22))
      | u_conj(FOL.IF(c11, c12, c13, inf1), FOL.IF(c21, c22, c23, inf2)) =
	 (u_conj(c11, c21); u_conj(c12, c22); u_conj(c13, c23))	 
      | u_conj(_, _) = does_not_unify()

    fun equalVarLists(varl1, varl2) =
      (zipapp(e_var, varl1, varl2); true)
      handle Unify => false

    fun unifyPatLists(patl1, patl2) =
      (trail := []; zipapp(u_pat, patl1, patl2); trail := []; true)
      handle Unify => false

    fun unifyConjs(conj1, conj2) =
      (trail := []; u_conj(conj1, conj2); trail := []; true)
      handle Unify => false

  end (* functor FOLUnifyFn *)
