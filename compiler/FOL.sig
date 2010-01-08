(* fol/fol.sig *)

signature FOL =
  sig

    structure Source	: SOURCE

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
				             } *
				             ident
				               

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

    val litEqual	: lit * lit -> bool
    val newvar		: ident -> var
    val deref		: var -> var
    
    val makeIdent	    : string * info -> ident
    val identName	    : ident -> string
    val identCtxInfo	: ident -> info
    val lidentName		: longid -> string
    val lidentCtxInfo	: longid -> info
    val dummyInfo       : info
    val dummyIdent		: ident
    

  end (* signature FOL *)
