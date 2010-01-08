(* foltocps/transenv.sml *)

functor TransEnvFn(structure Util : UTIL
		   structure StrDict : STR_DICT
		   structure CPS : CPS
		     ) : TRANSENV =
  struct

    structure StrDict	= StrDict
    structure ConRep	= CPS.ConRep
    structure CPS		= CPS

    datatype translation = DEF of {def: CPS.def, tieknot: unit->unit}
						 | LIT of CPS.literal
						 | CON of {rep: ConRep.conrep, nrcons: int}
						 | VAR of CPS.trivexp

    fun bug s = Util.bug("TransEnv."^s)

    fun unary_arg [x]	= x
      | unary_arg _	= bug "unary_arg"

    fun binary_args [x,y]= (x,y)
      | binary_args _	= bug "binary_args"

    (* This is a bloody awful hack! *)
    fun new_var() =
      let val (var as CPS.VAR{uses,...}) = CPS.newVar(CPS.dummyLongIdent)
      in
		uses := 10;
		var
      end

    fun mk_unary_inliner unop =
      fn{args, fc=_, sc} =>
		let val pos = CPS.ConRep.dummyInfo
		    val name = CPS.ConRep.dummyLongIdent
			val varx = new_var()
			and varz = new_var()
			val e = CPS.LETe(varx, unary_arg args,
						CPS.mkPRIMe(varz, CPS.UNARYp(unop, CPS.mkVARte varx),
						CPS.mkAppSCe{sc=sc,args=[CPS.mkVARte varz], name=name, pos=pos}))
		in
		  SOME e
		end

    val int_int_inliner =
      fn{args, fc=_, sc} =>
		let val pos = CPS.ConRep.dummyInfo
		    val name = CPS.ConRep.dummyLongIdent		
			val varx = new_var()
			val e = CPS.LETe(varx, unary_arg args,
						CPS.mkAppSCe{sc=sc,args=[CPS.mkVARte varx], name=name, pos=pos})
		in
		  SOME e
		end

    fun mk_binary_inliner binop =
      fn{args, fc=_, sc} =>
		let val pos = CPS.ConRep.dummyInfo
		    val name = CPS.ConRep.dummyLongIdent		
			val varx = new_var()
			and vary = new_var()
			and varz = new_var()
			and (arg1,arg2) = binary_args args
			val e = CPS.LETe(varx, arg1,
						CPS.mkLETe(vary, arg2,
							CPS.mkPRIMe(varz, CPS.BINARYp(binop,CPS.mkVARte varx,CPS.mkVARte vary),
								CPS.mkAppSCe{sc=sc,args=[CPS.mkVARte varz], name=name, pos=pos})))
		in
		  SOME e
		end

    fun mk_divmod_inliner binop =
      fn{args, fc, sc} =>
		let val pos = CPS.ConRep.dummyInfo
		    val name = CPS.ConRep.dummyLongIdent		
			val varx = new_var()
			and vary = new_var()
			and varz = new_var()
			val valx = CPS.mkVARte varx
			and valy = CPS.mkVARte vary	
			and valz = CPS.mkVARte varz
			and (arg1,arg2) = binary_args args
			val e0 = CPS.mkLETe(varx, arg1,
						 CPS.mkPRIMe(varz, CPS.BINARYp(binop,valx,valy),
						 CPS.mkAppSCe{sc=sc,args=[valz],name=name,pos=pos}))
			val e = CPS.LETe(vary, arg2, CPS.mkSWITCHe(valy, [(CPS.INTcon 0, CPS.mkAppFCe{fc=fc,name=name,pos=pos})], SOME e0))
		in
		  SOME e
		end

	fun mkId(str) = CPS.makeIdent(str, CPS.dummyInfo)
	fun mkLongId(str1, str2) = ConRep.LONGID{module=SOME(mkId(str1)), name=mkId(str2)}

    val tenv0 =
      let fun bind(te, name, proc) =
	    StrDict.insert(te, name, LIT(CPS.PROClit proc))
	  fun extern(te, name) =
	    bind(te, name, CPS.EXTERN_REL(ConRep.LONGID{module=SOME(mkId("RML")), name=mkId(name)}, NONE))
	  fun inline(te, name, inliner) =
	    bind(te, name, CPS.EXTERN_REL(ConRep.LONGID{module=SOME(mkId("RML")), name=mkId(name)}, SOME inliner))
	  val te = StrDict.empty
	  (* booleans *)
	  val te = StrDict.insert(te, "false", CON{rep=ConRep.INT 0, nrcons=2})
	  val te = StrDict.insert(te, "true", CON{rep=ConRep.INT 1, nrcons=2})
	  val te = inline(te, "bool_and", mk_binary_inliner CPS.BOOL_AND)
	  val te = inline(te, "bool_not", mk_unary_inliner CPS.BOOL_NOT)
	  val te = inline(te, "bool_or", mk_binary_inliner CPS.BOOL_OR)
	  (* options *)
	  val te = StrDict.insert(te, "NONE", CON{rep=ConRep.BOX{arity=0,tag=0,name=mkLongId("RML", "NONE")}, nrcons=2})
	  val te = StrDict.insert(te, "SOME", CON{rep=ConRep.BOX{arity=1,tag=1,name=mkLongId("RML", "SOME")}, nrcons=2})
	  (* characters *)
	  val te = inline(te, "char_int", int_int_inliner)
	  val te = inline(te, "int_char", int_int_inliner)
	  (* string characters *)
	  val te = extern(te, "string_char_int")
	  val te = extern(te, "int_string_char")
	  (* strings *)
	  val te = extern(te, "string_append")
	  val te = extern(te, "string_int")
	  val te = extern(te, "string_list")
	  val te = extern(te, "string_list_string_char") (* string to string char list  *)
	  val te = extern(te, "list_string")
	  val te = extern(te, "string_char_list_string") (* string char list to string *)
	  val te = extern(te, "string_length")
	  val te = extern(te, "string_nth")
	  val te = extern(te, "string_nth_string_char") (* string index returns string char *)	  
	  val te = extern(te, "string_get")
	  val te = extern(te, "string_get_string_char") (* string index returns string char *)	  
	  val te = extern(te, "string_setnth")
	  val te = extern(te, "string_setnth_string_char") (* string index string char => new str *)	  
	  val te = extern(te, "string_update")
	  val te = extern(te, "string_update_string_char") (* string index string char => new str *)
	  val te = extern(te, "string_equal")
	  val te = extern(te, "string_compare")
	  val te = extern(te, "string_append_list")
	  
	  (* immutable vectors *)
	  val te = extern(te, "vector_length")
	  val te = extern(te, "vector_nth")
	  val te = extern(te, "vector_get")	  
	  val te = extern(te, "vector_list")
	  val te = extern(te, "list_vector")
	  val te = extern(te, "vector_setnth")
	  val te = extern(te, "vector_update")
	  val te = extern(te, "vector_create")
      val te = extern(te, "vector_add")
      val te = extern(te, "vector_array")
      val te = extern(te, "vector_copy")

	  (* mutable arrays *)
	  val te = extern(te, "array_length")
	  val te = extern(te, "array_nth")
	  val te = extern(te, "array_get")	  
	  val te = extern(te, "array_list")
	  val te = extern(te, "list_array")
	  val te = extern(te, "array_setnth")
	  val te = extern(te, "array_update")	  
	  val te = extern(te, "array_create")
      val te = extern(te, "array_add")	  
      val te = extern(te, "array_vector")
      val te = extern(te, "array_copy")
      	  
	  (* integers *)
	  val te = inline(te, "int_add", mk_binary_inliner CPS.INT_ADD)
	  val te = inline(te, "int_sub", mk_binary_inliner CPS.INT_SUB)
	  val te = inline(te, "int_mul", mk_binary_inliner CPS.INT_MUL)
	  val te = inline(te, "int_div", mk_divmod_inliner CPS.INT_DIV)
	  val te = inline(te, "int_mod", mk_divmod_inliner CPS.INT_MOD)
	  val te = inline(te, "int_abs", mk_unary_inliner CPS.INT_ABS)
	  val te = inline(te, "int_neg", mk_unary_inliner CPS.INT_NEG)
	  val te = inline(te, "int_max", mk_binary_inliner CPS.INT_MAX)
	  val te = inline(te, "int_min", mk_binary_inliner CPS.INT_MIN)
	  val te = inline(te, "int_lt", mk_binary_inliner CPS.INT_LT)
	  val te = inline(te, "int_le", mk_binary_inliner CPS.INT_LE)
	  val te = inline(te, "int_eq", mk_binary_inliner CPS.INT_EQ)
	  val te = inline(te, "int_ne", mk_binary_inliner CPS.INT_NE)
	  val te = inline(te, "int_ge", mk_binary_inliner CPS.INT_GE)
	  val te = inline(te, "int_gt", mk_binary_inliner CPS.INT_GT)
	  val te = extern(te, "int_real")
	  val te = extern(te, "int_string")
	  (* reals *)
	  val te = extern(te, "real_add")
	  val te = extern(te, "real_sub")
	  val te = extern(te, "real_mul")
	  val te = extern(te, "real_div")
	  val te = extern(te, "real_mod")
	  val te = extern(te, "real_abs")
	  val te = extern(te, "real_neg")
	  val te = extern(te, "real_cos")
	  val te = extern(te, "real_sin")
	  val te = extern(te, "real_atan")
	  val te = extern(te, "real_exp")
	  val te = extern(te, "real_ln")
	  val te = extern(te, "real_floor")
	  val te = extern(te, "real_int")
	  val te = extern(te, "real_pow")
	  val te = extern(te, "real_sqrt")
	  val te = extern(te, "real_max")
	  val te = extern(te, "real_min")
	  val te = extern(te, "real_lt")
	  val te = extern(te, "real_le")
	  val te = extern(te, "real_eq")
	  val te = extern(te, "real_ne")
	  val te = extern(te, "real_ge")
	  val te = extern(te, "real_gt")
	  val te = extern(te, "real_string")
	  val te = extern(te, "real_asin")
	  val te = extern(te, "real_acos")
	  val te = extern(te, "real_atan2")
	  val te = extern(te, "real_cosh")
	  val te = extern(te, "real_log")
	  val te = extern(te, "real_log10")
	  val te = extern(te, "real_sinh")
	  val te = extern(te, "real_tanh")
	  
	  (* lists *)
	  val te = StrDict.insert(te, "nil", CON{rep=ConRep.BOX{arity=0,tag=0,name=mkLongId("RML", "nil")}, nrcons=2})
	  val te = StrDict.insert(te, "cons", CON{rep=ConRep.BOX{arity=2,tag=1,name=mkLongId("RML", "cons")}, nrcons=2})
	  val te = extern(te, "list_append")
	  val te = extern(te, "list_reverse")
	  val te = extern(te, "list_length")
	  val te = extern(te, "list_member")
	  val te = extern(te, "list_nth")
	  val te = extern(te, "list_delete")
	  (* logical variables *)
	  val te = extern(te, "lvar_get")
	  val te = extern(te, "lvar_new")
	  val te = extern(te, "lvar_set")
	  (* misc *)
	  val te = extern(te, "clock")
	  val te = extern(te, "print")
	  val te = extern(te, "tick")
	  (* if expressions *)
	  val te = extern(te, "if_exp")
	  val te = extern(te, "ifExp")	  
	  (* debug *)
	  val te = extern(te, "debug")
	  val te = extern(te, "debug_print")
	  val te = extern(te, "debug_show_depth")	  
	  (* specific methods for dealing with different parameter arity *)
	  val te = extern(te, "debug_push_in01")
	  val te = extern(te, "debug_push_in02")
	  val te = extern(te, "debug_push_in03")
	  val te = extern(te, "debug_push_in04")
	  val te = extern(te, "debug_push_in05")
	  val te = extern(te, "debug_push_in06")
	  val te = extern(te, "debug_push_in07")
	  val te = extern(te, "debug_push_in08")
	  val te = extern(te, "debug_push_in09")
	  val te = extern(te, "debug_push_in10")
	  val te = extern(te, "debug_push_in11")
	  val te = extern(te, "debug_push_in12")
	  val te = extern(te, "debug_push_in13")
	  val te = extern(te, "debug_push_in14")
	  val te = extern(te, "debug_push_in15")
	  val te = extern(te, "debug_push_in16")
	  (* specific methods for dealing with different parameter arity *)
	  val te = extern(te, "debug_push_out01")
	  val te = extern(te, "debug_push_out02")
	  val te = extern(te, "debug_push_out03")
	  val te = extern(te, "debug_push_out04")
	  val te = extern(te, "debug_push_out05")
	  val te = extern(te, "debug_push_out06")
	  val te = extern(te, "debug_push_out07")
	  val te = extern(te, "debug_push_out08")
	  val te = extern(te, "debug_push_out09")
	  val te = extern(te, "debug_push_out10")
	  val te = extern(te, "debug_push_out11")
	  val te = extern(te, "debug_push_out12")
	  val te = extern(te, "debug_push_out13")
	  val te = extern(te, "debug_push_out14")
	  val te = extern(te, "debug_push_out15")
	  val te = extern(te, "debug_push_out16")

      (* adrpo addded Java names for all RML relations 2005-11-10 *)	  
	  val te = inline(te, "boolAnd", mk_binary_inliner CPS.BOOL_AND)
	  val te = inline(te, "boolNot", mk_unary_inliner CPS.BOOL_NOT)
	  val te = inline(te, "boolOr", mk_binary_inliner CPS.BOOL_OR)
	  (* characters *)
	  val te = inline(te, "charInt", int_int_inliner)
	  val te = inline(te, "intChar", int_int_inliner)
	  (* string characters *)
	  val te = extern(te, "stringCharInt")	  
	  val te = extern(te, "intStringChar")	  	  
	  (* strings *)
	  val te = extern(te, "stringAppend")	  
	  val te = extern(te, "stringInt")
	  val te = extern(te, "stringList")
	  val te = extern(te, "stringListStringChar")	  
	  val te = extern(te, "listString")
	  val te = extern(te, "stringCharListString")	  
	  val te = extern(te, "stringLength")
	  val te = extern(te, "stringNth")
	  val te = extern(te, "stringNthStringChar")	  
	  val te = extern(te, "stringGet")
	  val te = extern(te, "stringGetStringChar") (* string index returns string char *)	  
	  val te = extern(te, "stringSetNth")
	  val te = extern(te, "stringSetNthStringChar") (* string index string char => new str *)	  
	  val te = extern(te, "stringUpdate")
	  val te = extern(te, "stringUpdateStringChar") (* string index string char => new str *)
	  val te = extern(te, "stringEqual")
	  val te = extern(te, "stringCompare")
	  val te = extern(te, "stringAppendList")
	  	  	  
	  (* immutable vectors *)
	  val te = extern(te, "vectorLength")
	  val te = extern(te, "vectorNth")
	  val te = extern(te, "vectorGet")	  
	  val te = extern(te, "vectorList")
	  val te = extern(te, "listVector")
	  val te = extern(te, "vectorSetNth")
	  val te = extern(te, "vectorUpdate")	  
	  val te = extern(te, "vectorCreate")
      val te = extern(te, "vectorAdd")
      val te = extern(te, "vectorArray")
      val te = extern(te, "vectorCopy")

	  (* mutable arrays *)
	  val te = extern(te, "arrayLength")
	  val te = extern(te, "arrayNth")
	  val te = extern(te, "arrayGet")	  
	  val te = extern(te, "arrayList")
	  val te = extern(te, "listArray")
	  val te = extern(te, "arraySetnth")
	  val te = extern(te, "arrayUpdate")	  
	  val te = extern(te, "arrayCreate")
      val te = extern(te, "arrayAdd")	  
      val te = extern(te, "arrayVector")
      val te = extern(te, "arrayCopy")
      	  
	  (* integers *)
	  val te = inline(te, "intAdd", mk_binary_inliner CPS.INT_ADD)
	  val te = inline(te, "intSub", mk_binary_inliner CPS.INT_SUB)
	  val te = inline(te, "intMul", mk_binary_inliner CPS.INT_MUL)
	  val te = inline(te, "intDiv", mk_divmod_inliner CPS.INT_DIV)
	  val te = inline(te, "intMod", mk_divmod_inliner CPS.INT_MOD)
	  val te = inline(te, "intAbs", mk_unary_inliner CPS.INT_ABS)
	  val te = inline(te, "intNeg", mk_unary_inliner CPS.INT_NEG)
	  val te = inline(te, "intMax", mk_binary_inliner CPS.INT_MAX)
	  val te = inline(te, "intMin", mk_binary_inliner CPS.INT_MIN)
	  val te = inline(te, "intLt", mk_binary_inliner CPS.INT_LT)
	  val te = inline(te, "intLe", mk_binary_inliner CPS.INT_LE)
	  val te = inline(te, "intEq", mk_binary_inliner CPS.INT_EQ)
	  val te = inline(te, "intNe", mk_binary_inliner CPS.INT_NE)
	  val te = inline(te, "intGe", mk_binary_inliner CPS.INT_GE)
	  val te = inline(te, "intGt", mk_binary_inliner CPS.INT_GT)
	  val te = extern(te, "intReal")
	  val te = extern(te, "intString")
	  (* reals *)
	  val te = extern(te, "realAdd")
	  val te = extern(te, "realSub")
	  val te = extern(te, "realMul")
	  val te = extern(te, "realDiv")
	  val te = extern(te, "realMod")
	  val te = extern(te, "realAbs")
	  val te = extern(te, "realNeg")
	  val te = extern(te, "realCos")
	  val te = extern(te, "realSin")
	  val te = extern(te, "realAtan")
	  val te = extern(te, "realExp")
	  val te = extern(te, "realLn")
	  val te = extern(te, "realFloor")
	  val te = extern(te, "realInt")
	  val te = extern(te, "realPow")
	  val te = extern(te, "realSqrt")
	  val te = extern(te, "realMax")
	  val te = extern(te, "realMin")
	  val te = extern(te, "realLt")
	  val te = extern(te, "realLe")
	  val te = extern(te, "realEq")
	  val te = extern(te, "realNe")
	  val te = extern(te, "realGe")
	  val te = extern(te, "realGt")
	  val te = extern(te, "realString")
	  val te = extern(te, "realAsin")
	  val te = extern(te, "realAcos")
	  val te = extern(te, "realAtan2")
	  val te = extern(te, "realCosh")
	  val te = extern(te, "realLog")
	  val te = extern(te, "realLog10")
	  val te = extern(te, "realSinh")
	  val te = extern(te, "realTanh")
	  (* lists *)
	  val te = extern(te, "listAppend")
	  val te = extern(te, "listReverse")
	  val te = extern(te, "listLength")
	  val te = extern(te, "listMember")
	  val te = extern(te, "listNth")
	  val te = extern(te, "listDelete")
	  (* logical variables *)
	  val te = extern(te, "lvarGet")
	  val te = extern(te, "lvarNew")
	  val te = extern(te, "lvarSet")
	  
      in
		te
      end

    val menv0 = StrDict.insert(StrDict.empty, "RML", tenv0)

  end (* functor TransEnvFn *)
