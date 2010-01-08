(* cps/cps.sml *)

functor CPSFn(
	structure Util : UTIL	
	structure Source : SOURCE	
	structure ConRep : CONREP
		) : CPS =
  struct

	structure Source = Source
    structure ConRep = ConRep
    
    type info = ConRep.info
    type ident =  ConRep.ident
    type longid =  ConRep.longid

    datatype constant = INTcon of int
			          | HDRcon of {len: int, con: int}
			          | REALcon of real
			          | STRINGcon of string

    datatype unop = FETCH of int
			      | BOOL_NOT | INT_NEG | INT_ABS

    datatype binop = EQUAL
			       | BOOL_AND | BOOL_OR
			       | INT_ADD | INT_SUB | INT_MUL | INT_DIV
			       | INT_MOD | INT_MAX | INT_MIN | INT_LT
			       | INT_LE | INT_EQ | INT_NE | INT_GE | INT_GT

    datatype var = VAR of {	tag		: int,
							uses	: int ref,
							subst	: trivexp' option ref,
							name	: longid }

    and lamkind	= FClk
			    | SClk of {v_tvs: var list}

    and literal	= CONSTlit of constant
			    | STRUCTlit of int * literal list * longid
			    | PROClit of proc
			    | EXTERNlit of longid

    and proc = EXTERN_REL of longid * ({args:trivexp list, fc:trivexp, sc:trivexp} -> exp' option) option
			 | LOCAL_REL of def

    and label = LAB of {	tag		: int,
							uses	: int ref,
							fvars	: var list ref,
							bvars	: var list,
							body	: exp,
							name	: longid,
							pos		: info }

    and trivexp' = VARte of var
			     | LAMte of {	tag		: int,
								fvars	: var list ref,
								kind	: lamkind,
								body	: exp,
								name	: longid,
								pos		: info }
			     | QUOTEte of literal

    and trivexp	= TE of trivexp' ref

    and primapp	= MARKERp
				| MKSTRUCTp of int * trivexp list
				| UNARYp of unop * trivexp
				| BINARYp of binop * trivexp * trivexp

    and exp' = AppFCe of {fc:trivexp, name:longid, pos:info}
			 | AppSCe of {sc:trivexp, args:trivexp list, name:longid, pos:info}
			 | AppPVe of {pv:trivexp, args:trivexp list, fc:trivexp, sc:trivexp, name:longid, pos:info}
			 | LetLABe of label * exp
			 | AppLABe of label * trivexp list
			 | RESTOREe of trivexp * exp
			 | LETe of var * trivexp * exp
			 | PRIMe of var * primapp * exp
			 | SWITCHe of trivexp * (constant * exp) list * exp option

			 

    and exp	= EXP of exp' ref

    and def	= DEF of {	
					name	: longid,
					uses	: int ref,
					v_tvs	: var list,
					v_fc	: var,
					v_sc	: var,
					body	: exp,
					pos		: info }

    datatype module	= MODULE of {	name	: string,
					ctors	: (longid * ConRep.conrep) list,
					xmods	: string list,
					values	: (longid * literal) list,
					defines	: def list,
					source	: Source.source	}

    fun constEqual(INTcon i1, INTcon i2) = i1 = i2
      | constEqual(HDRcon{len=len1,con=con1}, HDRcon{len=len2,con=con2}) =
	  (len1 = len2) andalso (con1 = con2)
      | constEqual(REALcon r1, REALcon r2) = Real.==(r1, r2)
      | constEqual(STRINGcon s1, STRINGcon s2) = s1 = s2
      | constEqual(_, _) = false

    fun prune_te'(te) =
      case te
		of VARte(VAR{subst,name=keepNAME,...})	=>
			(case !subst
			   of NONE			=> te
			| SOME te'		=> 
				let val teNEW' = 
						case te' of
							VARte(VAR{tag,uses,subst,name}) (* keep the old name! *)
							=> VARte(VAR{tag=tag, uses=uses, subst=subst, name=keepNAME})
						| _ => te'
					val te'' = prune_te' teNEW'
				in
				  subst := SOME te''; te''
				end)
		 | _				=> te

    fun getTE' te =
      let	fun pruneTE(te as (TE r)) = (r := prune_te'(!r); te)
			fun untagTE(TE r) = r
      in
		untagTE(pruneTE te)
      end

    fun getTE te = !(getTE' te)

    fun getExp(EXP r) = !r

    fun newDef{name, args, fc, sc, body, pos} =
      DEF{name=name, uses=ref 1, v_tvs=args, v_fc=fc, v_sc=sc, body=body, pos=pos}

    val dummyInfo  = ConRep.INFO(~1, ~1)
    val dummyIdent = ConRep.IDENT("$", dummyInfo)
    val dummyLongIdent = ConRep.LONGID{module=NONE, name=dummyIdent}
    
    fun identName(ConRep.IDENT(name,_)) = name
    fun longIdentName(ConRep.LONGID{module=SOME(ConRep.IDENT(name1,_)),name=ConRep.IDENT(name2,_)}) = name1^"."^name2
    |	longIdentName(ConRep.LONGID{module=NONE,name=ConRep.IDENT(name2,_)}) = name2    
        
    fun makeIdent(name, info) = ConRep.IDENT(name, info)        
    fun makeLongIdent(id1, id2) = ConRep.LONGID{module=id1, name=id2}
    
    fun newVar(lid) =
      VAR{tag=Util.tick(), uses=ref 0, subst=ref NONE, name=lid}

    fun newLam(kind, body, lid, info) =
      TE(ref(LAMte{tag=Util.tick(), fvars=ref[], kind=kind, body=body, name=lid, pos=info}))

    fun newLab(bvars, body, lid, info) =
      LAB{tag=Util.tick(), uses=ref 0, fvars=ref [], bvars=bvars, body=body, name=lid, pos=info}

    fun mkVARte(v) = TE(ref(VARte v))
    fun mkQUOTEte(lit) = TE(ref(QUOTEte lit))

    fun mkAppFCe{fc,name,pos} = EXP(ref(AppFCe{fc=fc,name=name,pos=pos}))
    fun mkAppSCe{sc,args,name,pos} = EXP(ref(AppSCe{sc=sc,args=args,name=name,pos=pos}))
    fun mkAppPVe{pv,args,fc,sc,name,pos} = EXP(ref(AppPVe{pv=pv,args=args,fc=fc,sc=sc,name=name,pos=pos}))
    fun mkLetLABe(lab,e) = EXP(ref(LetLABe(lab,e)))
    fun mkAppLABe(lab,t_star) = EXP(ref(AppLABe(lab,t_star)))
    fun mkRESTOREe(t,e) = EXP(ref(RESTOREe(t,e)))
    fun mkLETe(v,t,e) = EXP(ref(LETe(v,t,e)))
    fun mkPRIMe(v,p,e) = EXP(ref(PRIMe(v,p,e)))
    fun mkSWITCHe(t,cases,def) = EXP(ref(SWITCHe(t,cases,def)))

  end (* functor CPSFn *)
