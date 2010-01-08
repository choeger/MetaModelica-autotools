(* code/code.sig *)

signature CODE =
  sig

	structure Source    : SOURCE
    structure ConRep	: CONREP
    structure Mangle	: MANGLE

    datatype gvar_name	= SPgvn | FCgvn | SCgvn | ARGgvn of int
    datatype gvar_scope	= INTRAgvs | INTERgvs
    datatype gvar'	= GVARSTR of string | GVAR of {scope: gvar_scope, name: gvar_name} 

    eqtype gvar 
        
    sharing type gvar = gvar' 
    
    datatype lvar	= LVAR of {tag:int, name:ConRep.longid}

    datatype variable	= GLOvar of gvar
			| LOCvar of lvar

    datatype label	= LABEL of Mangle.name * ConRep.longid * ConRep.info

    datatype litname	= LITNAME of int
    datatype litref	= INTlr of int
			| HDRlr of {len: int, con: int}
			| LABELlr of label
			| EXTERNlr of label
			| REALlr of litname
			| STRINGlr of litname
			| STRUCTlr of litname
    datatype litdef	= REALld of real
			| STRINGld of string
			| STRUCTld of int * litref list

    datatype value	= VAR of variable
			| LITERAL of litref
			| OFFSET of value * int
			| FETCH of value
			| UNTAGPTR of value
			| TAGPTR of value
			| CALL of label * value list

    datatype casetag	= INTct of int
			| HDRct of {len: int, con: int}
			| REALct of real
			| STRINGct of string

    datatype gototarget = LOCALg of label
			| EXTERNg of label
			| VALUEg of value
			
    datatype gototype =	  FClk (* failure *) 
						| SClk (* success *)
						| NClk (* normal *)
						| LClk (* label to shared state *)
						| EClk (* external *)

    datatype code'	= GOTO of gototarget * int * ConRep.longid * ConRep.info * gototype
			| STORE of value * value * code
			| BIND of variable option * value * code
			| SWITCH of value * (casetag * code) list * code option

    and code		= CODE of {fvars: lvar list ref, code: code'}

    datatype labdef	= LABDEF of {	
					globalP	: bool,			(* is this label global? *)
					label	: label,		(* the label of this function *)
					varHP	: lvar,			(* heap pointer *)
					nalloc	: int,			(* how much to alloc for this label *)
					nargs	: int,			(* how many arguments this label has *)
					code	: code,			(* the code for this label *)
					pos 	: ConRep.info } (* position in the primary file. used for debugging  *)
					
	datatype position = POSITION of ConRep.info

    datatype module	= MODULE of {	modname	: string,
					ctors	: (string * ConRep.conrep) list,
					xmods	: string list,
					xlabs	: label list,
					xvals	: label list,
					values	: (label * litref) list,
					litdefs	: (litname * litdef) list,
					labdefs	: labdef list,
					source  : Source.source }

    val gvarString	: gvar -> string
    val lvarString	: lvar -> string

    val prGoto		: TextIO.outstream
			* (TextIO.outstream -> label -> unit)
			* (TextIO.outstream -> value -> unit)
			* gototarget
			* int
			-> unit

    val mklab		: string * ConRep.longid * ConRep.info -> label

    val mkGOTO		: gototarget * int * ConRep.longid * ConRep.info * gototype -> code
    val mkSTORE		: value * value * code -> code
    val mkBIND		: variable option * value * code -> code
    val mkSWITCH	: value * (casetag * code) list * code option -> code

    val intraSP		: variable
    val intraFC		: variable
    val intraSC		: variable
    val intraArgs	: variable Vector.vector

    val interSP		: variable
    val interFC		: variable
    val interSC		: variable
    val interArgs	: variable Vector.vector

    val primMARKER	: label
    val primUNWIND	: label
    val primEQUAL	: label
    val primBOOL_NOT	: label
    val primINT_NEG	: label
    val primINT_ABS	: label
    val primBOOL_AND	: label
    val primBOOL_OR	: label
    val primINT_ADD	: label
    val primINT_SUB	: label
    val primINT_MUL	: label
    val primINT_DIV	: label
    val primINT_MOD	: label
    val primINT_MAX	: label
    val primINT_MIN	: label
    val primINT_LT	: label
    val primINT_LE	: label
    val primINT_EQ	: label
    val primINT_NE	: label
    val primINT_GE	: label
    val primINT_GT	: label
    
  end (* signature CODE *)
