(* fol/fol.sml *)

functor FOLFn(
	structure Util : UTIL
	structure Source: SOURCE
	) : FOL =
  struct

    structure Source	= Source
    
    (* pos start, pos end *)
	datatype info = INFO of int * int

    datatype ident	= IDENT of string * info

    datatype longid	= LONGID of ident option * ident * info

    datatype lit	= ICON of int
			| RCON of real
			| SCON of string

    datatype var	= VAR of { 
                               name	: string, 
                               uses	: int ref,
				               inst	: var option ref 
				             } * ident
				              
    datatype pat'	= WILDpat
			| LITpat of lit
			| CONpat of longid
			| STRUCTpat of longid option * pat list
    and pat		= PAT of var * pat'

    datatype varref	= GVAR of longid
			| BVAR of var

    datatype exp	= LITexp of lit
			| CONexp of longid
			| VARexp of varref
			| STRUCTexp of longid option * exp list

    datatype conj	= CALL of varref * exp list * var list * info
			| MATCH of (var * pat) list * info
			| EQUAL of var * exp * info
			| BIND of var * exp * info
			| NOT of conj * info
			| AND of conj * conj * info
			| IF of conj * conj * conj * info

    datatype disj	= RETURN of exp list * info
			| FAIL of info
			| ORELSE of disj * disj * info
			| ANDTHEN of conj * disj * info
			| COND of conj * disj * disj * info
			| CASE of var list * (pat list * disj) list * info

    datatype rel	= REL of ident * var list * disj * info

    datatype condesc	= CONcd of ident
			| CTORcd of ident * int
    datatype datdesc	= DATDESC of ident * condesc list

    datatype spec	= WITHspec of interface
			| DATAspec of datdesc list
			| VALspec of ident
			| RELspec of ident
    and interface	= INTERFACE of ident * spec list

    datatype dec	= WITHdec of interface
			| DATAdec of datdesc list
			| VALdec of ident * exp
			| RELdec of rel list

    datatype module	= MODULE of interface * dec list * Source.source

	val debugFlag = false
	fun debug s = if (debugFlag) then Util.outStdErr("FOLFn."^s) else ()

    val dummyInfo = INFO(~1, ~1)
    val dummyIdent = IDENT("$", dummyInfo)    

    fun makeIdent(name, info) = IDENT(name, info)
    fun rmlIdent name = IDENT(name, dummyInfo)
    fun identName(IDENT(name,_)) = name
    fun identCtxInfo(IDENT(_, info)) = info
    fun lidentName(LONGID(SOME(IDENT(name1,_)),IDENT(name2,_),_)) = name1^"."^name2
    |	lidentName(LONGID(NONE,IDENT(name,_),_)) = name
    fun lidentCtxInfo(LONGID(_, _, info)) = info

    fun litEqual(ICON i1, ICON i2) = i1=i2
      | litEqual(RCON r1, RCON r2) = Real.==(r1,r2)
      | litEqual(SCON s1, SCON s2) = s1=s2
      | litEqual(_, _) = false

    fun newvar(src) = VAR({name="$"^Int.toString(Util.tick()), uses=ref 0, inst=ref NONE}, src)

    fun deref(var as VAR({inst,name,...}, id)) =
    (
      debug("deref: Dereferencing: "^name^":"^identName(id)^"\n");
      case !inst
		of SOME var	=> (debug("deref: -> "); deref var)
		 | NONE		=> (debug("deref: =>| "); var)
	)

  end (* functor FOLFn *)
