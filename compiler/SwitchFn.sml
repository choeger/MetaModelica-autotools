(* code/switch.sml *)

functor SwitchFn(
	structure MakeString : MAKESTRING
	structure Source : SOURCE
	structure ConRep : CONREP
	structure Mangle : MANGLE
		   ) : SWITCH =
  struct

	structure Source    = Source
    structure ConRep	= ConRep
    structure Mangle	= Mangle

    datatype gvar_name	= SPgvn | FCgvn | SCgvn | ARGgvn of int
    datatype gvar_scope	= INTRAgvs | INTERgvs
    datatype gvar'	= GVARSTR of string | GVAR of {scope: gvar_scope, name: gvar_name}

    type gvar		= gvar'
    
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
					globalP	: bool,
					label	: label,
					varHP	: lvar,
					nalloc	: int,
					nargs	: int,
					code	: code,
					pos 	: ConRep.info }

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

    fun gvarString(GVAR{scope,name}) =
      let fun gvs2str(INTRAgvs) = "intra"
	    | gvs2str(INTERgvs) = "rml"
	  fun gvn2str(SPgvn) = "SP"
	    | gvn2str(FCgvn) = "FC"
	    | gvn2str(SCgvn) = "SC"
	    | gvn2str(ARGgvn i) = "A" ^ MakeString.icvt i
      in
		gvs2str scope ^ gvn2str name
      end

    fun lvarString(LVAR{tag,name}) = "tmp" ^ (MakeString.icvt tag) ^ " " ^ ConRep.fixName(name)

    fun prGoto(os, prLabel, prValue, target, _) = (* nargs ignored *)
      case target
	of LOCALg label =>
	    (TextIO.output(os, "\n\tgoto label__");
	     prLabel os label;
	     TextIO.output(os, ";"))
	 | EXTERNg label =>
	    (TextIO.output(os, "\n\ttheLabel = &");
	     prLabel os label;
	     TextIO.output(os, ";\n\tgoto epilogue;"))
	 | VALUEg value =>
	    (TextIO.output(os, "\n\ttheLabel = (const rml_label_t*)(");
	     prValue os value;
	     TextIO.output(os, ");\n\tgoto mask_dispatch;"))

    fun mklab(str, name, info) = LABEL(Mangle.encode str, name, info)

    fun mkcode c = CODE{fvars=ref[], code=c}
    fun mkGOTO(target, nargs, name, pos, gototype) = mkcode(GOTO(target, nargs, name, pos, gototype))
    fun mkSTORE(v1,v2,c) = mkcode(STORE(v1,v2,c))
    fun mkBIND(xopt,v,c) = mkcode(BIND(xopt,v,c))
    fun mkSWITCH(v,cases,def) = mkcode(SWITCH(v,cases,def))

    val intraSP		= GLOvar(GVAR{scope=INTRAgvs, name=SPgvn})
    val intraFC		= GLOvar(GVAR{scope=INTRAgvs, name=FCgvn})
    val intraSC		= GLOvar(GVAR{scope=INTRAgvs, name=SCgvn})
    val intraArgs	=
      Vector.fromList[	GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 0}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 1}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 2}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 3}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 4}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 5}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 6}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 7}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 8}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 9}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 10}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 11}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 12}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 13}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 14}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 15}),
			(* 2004-09-28 adrpo added 16 more parameters*)
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 16}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 17}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 18}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 19}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 20}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 21}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 22}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 23}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 24}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 25}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 26}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 27}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 28}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 29}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 30}),
			GLOvar(GVAR{scope=INTRAgvs, name=ARGgvn 31}) ]

    val interSP		= GLOvar(GVAR{scope=INTERgvs, name=SPgvn})
    val interFC		= GLOvar(GVAR{scope=INTERgvs, name=FCgvn})
    val interSC		= GLOvar(GVAR{scope=INTERgvs, name=SCgvn})
    val interArgs	=
      Vector.fromList[	GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 0}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 1}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 2}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 3}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 4}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 5}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 6}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 7}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 8}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 9}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 10}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 11}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 12}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 13}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 14}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 15}),
			(* 2004-09-28 adrpo added 16 more parameters*)
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 16}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 17}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 18}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 19}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 20}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 21}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 22}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 23}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 24}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 25}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 26}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 27}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 28}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 29}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 30}),
			GLOvar(GVAR{scope=INTERgvs, name=ARGgvn 31}) ]

    (* These internal names are NOT mangled *)

    val primMARKER	= LABEL(Mangle.NAME "rml_prim_marker", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primUNWIND	= LABEL(Mangle.NAME "rml_prim_unwind", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primEQUAL	= LABEL(Mangle.NAME "rml_prim_equal", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primBOOL_NOT	= LABEL(Mangle.NAME "RML_PRIM_BOOL_NOT", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_NEG	= LABEL(Mangle.NAME "RML_PRIM_INT_NEG", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_ABS	= LABEL(Mangle.NAME "RML_PRIM_INT_ABS", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primBOOL_AND	= LABEL(Mangle.NAME "RML_PRIM_BOOL_AND", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primBOOL_OR	= LABEL(Mangle.NAME "RML_PRIM_BOOL_OR", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_ADD	= LABEL(Mangle.NAME "RML_PRIM_INT_ADD", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_SUB	= LABEL(Mangle.NAME "RML_PRIM_INT_SUB", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_MUL	= LABEL(Mangle.NAME "RML_PRIM_INT_MUL", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_DIV	= LABEL(Mangle.NAME "RML_PRIM_INT_DIV", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_MOD	= LABEL(Mangle.NAME "RML_PRIM_INT_MOD", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_MAX	= LABEL(Mangle.NAME "RML_PRIM_INT_MAX", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_MIN	= LABEL(Mangle.NAME "RML_PRIM_INT_MIN", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_LT	= LABEL(Mangle.NAME "RML_PRIM_INT_LT", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_LE	= LABEL(Mangle.NAME "RML_PRIM_INT_LE", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_EQ	= LABEL(Mangle.NAME "RML_PRIM_INT_EQ", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_NE	= LABEL(Mangle.NAME "RML_PRIM_INT_NE", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_GE	= LABEL(Mangle.NAME "RML_PRIM_INT_GE", ConRep.dummyLongIdent, ConRep.dummyInfo)
    val primINT_GT	= LABEL(Mangle.NAME "RML_PRIM_INT_GT", ConRep.dummyLongIdent, ConRep.dummyInfo)

  end (* functor SwitchFn *)
