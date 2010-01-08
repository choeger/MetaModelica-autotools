(* cps/cps.sig *)

signature CPS =
  sig

	structure Source    : SOURCE
    structure ConRep	: CONREP
    type info = ConRep.info 
	type ident = ConRep.ident
	type longid = ConRep.longid
	    
    datatype constant = INTcon of int
			          | HDRcon of {len: int, con: int}
			          | REALcon of real
			          | STRINGcon of string

    datatype unop	= FETCH of int
			| BOOL_NOT | INT_NEG | INT_ABS

    datatype binop	= EQUAL
			| BOOL_AND | BOOL_OR
			| INT_ADD | INT_SUB | INT_MUL | INT_DIV
			| INT_MOD | INT_MAX | INT_MIN | INT_LT
			| INT_LE | INT_EQ | INT_NE | INT_GE | INT_GT

    datatype var = VAR of {	tag	: int,
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

    and label = LAB of {	tag 	: int,
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

    and trivexp		= TE of trivexp' ref

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

    and exp		= EXP of exp' ref

    and def		= DEF of {	
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

    val constEqual	: constant * constant -> bool

    val getTE'		: trivexp -> trivexp' ref
    val getTE		: trivexp -> trivexp'

    val getExp		: exp -> exp'

    val newVar		: longid -> var
    val newDef		: {name:longid, args:var list, fc:var, sc:var, body:exp, pos:info} -> def
    val newLam		: lamkind  * exp * longid * info -> trivexp
    val newLab		: var list * exp * longid * info -> label

    val mkVARte		: var -> trivexp
    val mkQUOTEte	: literal -> trivexp

    val mkAppFCe	: {fc:trivexp, name:longid, pos:info} -> exp
    val mkAppSCe	: {sc:trivexp, args:trivexp list, name:longid, pos:info} -> exp
    val mkAppPVe	: {pv:trivexp, args:trivexp list, fc:trivexp, sc:trivexp, name:longid, pos:info} -> exp
    val mkLetLABe	: label * exp -> exp
    val mkAppLABe	: label * trivexp list -> exp
    val mkRESTOREe	: trivexp * exp -> exp
    val mkLETe		: var * trivexp * exp -> exp
    val mkPRIMe		: var * primapp * exp -> exp
    val mkSWITCHe	: trivexp * (constant*exp) list * exp option -> exp
    
    val makeIdent	    : string * info -> ident
    val makeLongIdent	: ident option * ident -> longid
    
    val identName	    : ident  -> string
    val longIdentName   : longid -> string    
    
    val dummyInfo       : info
    val dummyIdent		: ident 
    
    val dummyLongIdent  : longid   

  end (* signature CPS *)
