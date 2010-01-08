(* static/statobj.sml *)

functor StatObjFn(
	structure Absyn : ABSYN
	structure TyFcn : TY_FCN
	structure TyScheme : TY_SCHEME
	structure Control  : CONTROL
	sharing TyFcn.Ty = TyScheme.Ty
	sharing type Absyn.IdentDict.Key.ord_key = Absyn.ident	
		    ) : STAT_OBJ =
  struct

    structure Absyn	= Absyn
    structure IdentDict = Absyn.IdentDict
    structure Ty	= TyFcn.Ty
    structure TyFcn	= TyFcn
    structure TyScheme	= TyScheme

    datatype valkind	= CON | VAR | REL
    datatype valstr	= VALSTR of 
			{
				vk: valkind, 
				sigma: TyScheme.tyscheme, 
				localVE: valstr Absyn.IdentDict.dict,
				global: bool
			}
			
    datatype tystr	= TYSTR of {theta: TyFcn.tyfcn,
				    abstract: bool}
    datatype modstr	= MODSTR of {TE: tystr IdentDict.dict,
				     VE: valstr IdentDict.dict,
				     source: Absyn.Source.source}

    (* standard type names *)
    val t_bool	= Ty.TYNAME{modid="RML",tycon="bool",eq=ref Ty.MAYBE}
    val t_char	= Ty.TYNAME{modid="RML",tycon="char",eq=ref Ty.MAYBE}
    val t_int	  = Ty.TYNAME{modid="RML",tycon="int",eq=ref Ty.MAYBE}
    val t_list	= Ty.TYNAME{modid="RML",tycon="list",eq=ref Ty.MAYBE}
    val t_lvar	= Ty.TYNAME{modid="RML",tycon="lvar",eq=ref Ty.ALWAYS}
    val t_option= Ty.TYNAME{modid="RML",tycon="option",eq=ref Ty.MAYBE}
    val t_real	= Ty.TYNAME{modid="RML",tycon="real",eq=ref Ty.MAYBE}
    val t_string= Ty.TYNAME{modid="RML",tycon="string",eq=ref Ty.MAYBE}
    val t_vector= Ty.TYNAME{modid="RML",tycon="vector",eq=ref Ty.MAYBE}
    val t_array = Ty.TYNAME{modid="RML",tycon="array",eq=ref Ty.MAYBE}
    
    (* standard atomic types *)
    val tau_bool	= Ty.CONS([],t_bool)
    val tau_char	= Ty.CONS([],t_char)
    val tau_int		= Ty.CONS([],t_int)
    val tau_real	= Ty.CONS([],t_real)
    val tau_string= Ty.CONS([],t_string)

    (* some standard types *)
    val alpha		= Ty.RIGID "a"
    val tau_alpha	= Ty.VAR alpha
    val beta		= Ty.RIGID "b"
    val tau_beta	= Ty.VAR beta
    val tau_alpha_list	= Ty.CONS([tau_alpha], t_list)
    val tau_beta_list	= Ty.CONS([tau_beta],  t_list)    
    val tau_alpha_lvar	= Ty.CONS([tau_alpha], t_lvar)
    val tau_alpha_option= Ty.CONS([tau_alpha], t_option)
    val tau_alpha_vector= Ty.CONS([tau_alpha], t_vector)
    val tau_alpha_array = Ty.CONS([tau_alpha], t_array)    
    val tau_char_list	= Ty.CONS([tau_char], t_list)
    val tau_string_list	= Ty.CONS([tau_string], t_list)
    val eqalpha		= Ty.RIGID "'a"
    val tau_eqalpha	= Ty.VAR eqalpha
    val tau_eqalpha_list= Ty.CONS([tau_eqalpha], t_list)
	val tau_debug_param = [tau_string, tau_alpha]
	
	fun getFreshTau(x) = [tau_string, Ty.VAR(Ty.RIGID("rmldb_var_x"^Int.toString(x)))]
	
	fun makeParameterTyList(foo, nelts) =
		if (nelts = 0) 
		then foo
		else makeParameterTyList(foo @ getFreshTau(nelts), nelts-1) (* add one more to the list *)
	(* --- end adrpo added 2004-11-23 *)
	
    (* Source object for the standard environment *)
    val sourceInit =
      Absyn.Source.SOURCE(
		ref(Absyn.Source.ArraySourceMap.new("(pervasive RML environment)",Absyn.Source.getCurrentDate())))

    (* standard value environment *)
    val VE_init =
      let fun bind(VE, id, tau, vk) =
	    let val sigma = TyScheme.genAll tau
		val id = Absyn.rmlIdent id
	    in
	      IdentDict.insert(VE, id, VALSTR{vk=vk, sigma=sigma, localVE=IdentDict.empty, global=true})
	    end
	  fun bindRel(VE, var, domtaus, codtaus) =
	    bind(VE, var, Ty.REL(domtaus,codtaus), REL)
	  fun bindCon(VE, con, tau) = bind(VE, con, tau, CON)
	  val VE = IdentDict.empty
	  (* options *)
	  val VE = bindCon(VE,"NONE",tau_alpha_option)
	  val VE = bindCon(VE,"SOME",Ty.REL([tau_alpha],[tau_alpha_option]))
	  (* booleans *)
	  val VE = bindCon(VE,"false",tau_bool)
	  val VE = bindCon(VE,"true",tau_bool)
	  val VE = bindRel(VE,"bool_and",[tau_bool,tau_bool],[tau_bool])
	  val VE = bindRel(VE,"bool_not",[tau_bool],[tau_bool])
	  val VE = bindRel(VE,"bool_or",[tau_bool,tau_bool],[tau_bool])
	  (* characters *)
	  val VE = bindRel(VE,"char_int",[tau_char],[tau_int])
	  val VE = bindRel(VE,"string_char_int",[tau_string],[tau_int])
	  (* integers *)
	  val VE = bindRel(VE,"int_abs",[tau_int],[tau_int])
	  val VE = bindRel(VE,"int_add",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_char",[tau_int],[tau_char])
	  val VE = bindRel(VE,"int_string_char",[tau_int],[tau_string])
	  val VE = bindRel(VE,"int_div",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_eq",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_ge",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_gt",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_le",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_lt",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_max",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_min",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_mod",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_mul",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"int_ne",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"int_neg",[tau_int],[tau_int])
	  val VE = bindRel(VE,"int_real",[tau_int],[tau_real])
	  val VE = bindRel(VE,"int_string",[tau_int],[tau_string])
	  val VE = bindRel(VE,"int_sub",[tau_int,tau_int],[tau_int])
	  (* lists *)
	  val VE = bindCon(VE,"cons",Ty.REL([tau_alpha,tau_alpha_list],[tau_alpha_list]))
	  val VE = bindCon(VE,"nil",tau_alpha_list)
	  val VE = bindRel(VE,"list_append",[tau_alpha_list,tau_alpha_list],[tau_alpha_list])
	  val VE = bindRel(VE,"list_delete",[tau_alpha_list,tau_int],[tau_alpha_list])
	  val VE = bindRel(VE,"list_length",[tau_alpha_list],[tau_int])
	  val VE = bindRel(VE,"list_member",[tau_eqalpha,tau_eqalpha_list],[tau_bool])
	  val VE = bindRel(VE,"list_nth",[tau_alpha_list,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"list_reverse",[tau_alpha_list],[tau_alpha_list])
	  val VE = bindRel(VE,"list_string",[tau_char_list],[tau_string])
	  val VE = bindRel(VE,"string_char_list_string",[tau_string_list],[tau_string])	  
	  val VE = bindRel(VE,"list_vector",[tau_alpha_list],[tau_alpha_vector])
	  val VE = bindRel(VE,"list_array",[tau_alpha_list],[tau_alpha_array])	  
	  val VE = bindRel(VE,"list_map",[tau_alpha_list,Ty.REL([tau_alpha],[tau_beta])],[tau_beta_list])	  
	  (* logical variables *)
	  val VE = bindRel(VE,"lvar_get",[tau_alpha_lvar],[tau_alpha_option])
	  val VE = bindRel(VE,"lvar_new",[],[tau_alpha_lvar])
	  val VE = bindRel(VE,"lvar_set",[tau_alpha_lvar,tau_alpha],[])
	  (* reals *)
	  val VE = bindRel(VE,"real_abs",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_add",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_atan",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_cos",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_div",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_eq",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_exp",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_floor",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_ge",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_gt",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_int",[tau_real],[tau_int])
	  val VE = bindRel(VE,"real_le",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_ln",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_lt",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_max",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_min",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_mod",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_mul",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_ne",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"real_neg",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_pow",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_sin",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_sqrt",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_string",[tau_real],[tau_string])
	  val VE = bindRel(VE,"real_sub",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"real_asin",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_acos",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_atan2",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_cosh",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_log",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_log10",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_sinh",[tau_real],[tau_real])
	  val VE = bindRel(VE,"real_tanh",[tau_real],[tau_real])
	  (* strings *)
	  val VE = bindRel(VE,"string_append",[tau_string,tau_string],[tau_string])
	  val VE = bindRel(VE,"string_int",[tau_string],[tau_int])
	  val VE = bindRel(VE,"string_length",[tau_string],[tau_int])
	  val VE = bindRel(VE,"string_list",[tau_string],[tau_char_list])
	  val VE = bindRel(VE,"string_list_string_char",[tau_string],[tau_string_list])	  
	  val VE = bindRel(VE,"string_nth",[tau_string,tau_int],[tau_char])
	  val VE = bindRel(VE,"string_nth_string_char",[tau_string,tau_int],[tau_string])
	  val VE = bindRel(VE,"string_get",[tau_string,tau_int],[tau_char])
	  val VE = bindRel(VE,"string_get_string_char",[tau_string,tau_int],[tau_string])
	  val VE = bindRel(VE,"string_setnth",[tau_string,tau_int,tau_char],[tau_string])
	  val VE = bindRel(VE,"string_setnth_string_char",[tau_string,tau_int,tau_string],[tau_string])
	  val VE = bindRel(VE,"string_update",[tau_string,tau_int,tau_char],[tau_string])	  
	  val VE = bindRel(VE,"string_update_string_char",[tau_string,tau_int,tau_string],[tau_string])	  
	  val VE = bindRel(VE,"string_equal",[tau_string,tau_string],[tau_bool])	  
	  val VE = bindRel(VE,"string_compare",[tau_string,tau_string],[tau_int])
	  val VE = bindRel(VE,"string_append_list",[tau_string_list],[tau_string])
	  	  
	  (* immutable vectors *)
	  val VE = bindRel(VE,"vector_length",[tau_alpha_vector],[tau_int])
	  val VE = bindRel(VE,"vector_list",[tau_alpha_vector],[tau_alpha_list])
	  val VE = bindRel(VE,"vector_nth",[tau_alpha_vector,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"vector_get",[tau_alpha_vector,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"vector_setnth",[tau_alpha_vector,tau_int,tau_alpha],[tau_alpha_vector])
	  val VE = bindRel(VE,"vector_update",[tau_alpha_vector,tau_int,tau_alpha],[tau_alpha_vector])	  
	  val VE = bindRel(VE,"vector_create",[tau_int,tau_alpha],[tau_alpha_vector])
      val VE = bindRel(VE,"vector_add",[tau_alpha_vector,tau_alpha],[tau_alpha_vector])
      val VE = bindRel(VE,"vector_array",[tau_alpha_vector],[tau_alpha_array])      
      val VE = bindRel(VE,"vector_copy",[tau_alpha_vector],[tau_alpha_vector])      

	  (* mutable arrays *)      
	  val VE = bindRel(VE,"array_length",[tau_alpha_array],[tau_int])
	  val VE = bindRel(VE,"array_list",[tau_alpha_array],[tau_alpha_list])
	  val VE = bindRel(VE,"array_nth",[tau_alpha_array,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"array_get",[tau_alpha_array,tau_int],[tau_alpha])	  
	  val VE = bindRel(VE,"array_setnth",[tau_alpha_array,tau_int,tau_alpha],[tau_alpha_array])
	  val VE = bindRel(VE,"array_update",[tau_alpha_array,tau_int,tau_alpha],[tau_alpha_array])	  
	  val VE = bindRel(VE,"array_create",[tau_int, tau_alpha],[tau_alpha_array])
      val VE = bindRel(VE,"array_add",[tau_alpha_array,tau_alpha],[tau_alpha_array])
      val VE = bindRel(VE,"array_vector",[tau_alpha_array],[tau_alpha_vector])
      val VE = bindRel(VE,"array_copy",[tau_alpha_array],[tau_alpha_array])      
      
      (* if expressions *)
      val VE = bindRel(VE,"if_exp",[tau_bool,tau_alpha,tau_alpha],[tau_alpha])
            
      (* the environment using Java name convention 2005-11-10 *)	  
	  val VE = bindRel(VE,"boolAnd",[tau_bool,tau_bool],[tau_bool])
	  val VE = bindRel(VE,"boolNot",[tau_bool],[tau_bool])
	  val VE = bindRel(VE,"boolOr",[tau_bool,tau_bool],[tau_bool])
	  (* characters *)
	  val VE = bindRel(VE,"charInt",[tau_char],[tau_int])
	  val VE = bindRel(VE,"stringCharInt",[tau_string],[tau_int])
	  (* integers *)
	  val VE = bindRel(VE,"intAbs",[tau_int],[tau_int])	  
	  val VE = bindRel(VE,"intAdd",[tau_int,tau_int],[tau_int])	  
	  val VE = bindRel(VE,"intChar",[tau_int],[tau_char])
	  val VE = bindRel(VE,"intStringChar",[tau_int],[tau_string])
	  val VE = bindRel(VE,"intDiv",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"intEq",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intGe",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intGt",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intLe",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intLt",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intMax",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"intMin",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"intMod",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"intMul",[tau_int,tau_int],[tau_int])
	  val VE = bindRel(VE,"intNe",[tau_int,tau_int],[tau_bool])
	  val VE = bindRel(VE,"intNeg",[tau_int],[tau_int])
	  val VE = bindRel(VE,"intReal",[tau_int],[tau_real])
	  val VE = bindRel(VE,"intString",[tau_int],[tau_string])
	  val VE = bindRel(VE,"intSub",[tau_int,tau_int],[tau_int])
	  (* lists *)
	  val VE = bindRel(VE,"listAppend",[tau_alpha_list,tau_alpha_list],[tau_alpha_list])
	  val VE = bindRel(VE,"listDelete",[tau_alpha_list,tau_int],[tau_alpha_list])
	  val VE = bindRel(VE,"listLength",[tau_alpha_list],[tau_int])
	  val VE = bindRel(VE,"listMember",[tau_eqalpha,tau_eqalpha_list],[tau_bool])
	  val VE = bindRel(VE,"listNth",[tau_alpha_list,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"listReverse",[tau_alpha_list],[tau_alpha_list])
	  val VE = bindRel(VE,"listString",[tau_char_list],[tau_string])
	  val VE = bindRel(VE,"stringCharListString",[tau_string_list],[tau_string])
	  val VE = bindRel(VE,"listVector",[tau_alpha_list],[tau_alpha_vector])
	  val VE = bindRel(VE,"listArray",[tau_alpha_list],[tau_alpha_array])	  
	  val VE = bindRel(VE,"listMap",[tau_alpha_list,Ty.REL([tau_alpha],[tau_beta])],[tau_beta_list])	  
	  (* logical variables *)
	  val VE = bindRel(VE,"lvarGet",[tau_alpha_lvar],[tau_alpha_option])
	  val VE = bindRel(VE,"lvarNew",[],[tau_alpha_lvar])
	  val VE = bindRel(VE,"lvarSet",[tau_alpha_lvar,tau_alpha],[])
	  (* reals *)
	  val VE = bindRel(VE,"realAbs",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realAdd",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realAtan",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realCos",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realDiv",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realEq",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realExp",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realFloor",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realGe",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realGt",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realInt",[tau_real],[tau_int])
	  val VE = bindRel(VE,"realLe",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realLn",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realLt",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realMax",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realMin",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realMod",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realMul",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realNe",[tau_real,tau_real],[tau_bool])
	  val VE = bindRel(VE,"realNeg",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realPow",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realSin",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realSqrt",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realString",[tau_real],[tau_string])
	  val VE = bindRel(VE,"realSub",[tau_real,tau_real],[tau_real])
	  val VE = bindRel(VE,"realAsin",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realAcos",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realAtan2",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realCosh",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realLog",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realLog10",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realSinh",[tau_real],[tau_real])
	  val VE = bindRel(VE,"realTanh",[tau_real],[tau_real])
	  (* strings *)
	  val VE = bindRel(VE,"stringAppend",[tau_string,tau_string],[tau_string])
	  val VE = bindRel(VE,"stringInt",[tau_string],[tau_int])
	  val VE = bindRel(VE,"stringLength",[tau_string],[tau_int])
	  val VE = bindRel(VE,"stringList",[tau_string],[tau_char_list])
	  val VE = bindRel(VE,"stringListStringChar",[tau_string],[tau_string_list])
	  val VE = bindRel(VE,"stringNth",[tau_string,tau_int],[tau_char])
	  val VE = bindRel(VE,"stringNthStringChar",[tau_string,tau_int],[tau_string])
	  val VE = bindRel(VE,"stringGet",[tau_string,tau_int],[tau_char])
	  val VE = bindRel(VE,"stringGetStringChar",[tau_string,tau_int],[tau_string])
	  val VE = bindRel(VE,"stringSetNth",[tau_string,tau_int,tau_char],[tau_string])
	  val VE = bindRel(VE,"stringSetNthStringChar",[tau_string,tau_int,tau_string],[tau_string])
	  val VE = bindRel(VE,"stringUpdate",[tau_string,tau_int,tau_char],[tau_string])	  
	  val VE = bindRel(VE,"stringUpdateStringChar",[tau_string,tau_int,tau_string],[tau_string])	  
	  val VE = bindRel(VE,"stringEqual",[tau_string,tau_string],[tau_bool])	  
	  val VE = bindRel(VE,"stringCompare",[tau_string,tau_string],[tau_int])
	  val VE = bindRel(VE,"stringAppendList",[tau_string_list],[tau_string])

	  (* immutable vectors *)
	  val VE = bindRel(VE,"vectorLength",[tau_alpha_vector],[tau_int])
	  val VE = bindRel(VE,"vectorList",[tau_alpha_vector],[tau_alpha_list])
	  val VE = bindRel(VE,"vectorNth",[tau_alpha_vector,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"vectorGet",[tau_alpha_vector,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"vectorSetNth",[tau_alpha_vector,tau_int,tau_alpha],[tau_alpha_vector])
	  val VE = bindRel(VE,"vectorUpdate",[tau_alpha_vector,tau_int,tau_alpha],[tau_alpha_vector])	  
	  val VE = bindRel(VE,"vectorCreate",[tau_int,tau_alpha],[tau_alpha_vector])
      val VE = bindRel(VE,"vectorAdd",[tau_alpha_vector,tau_alpha],[tau_alpha_vector])
      val VE = bindRel(VE,"vectorArray",[tau_alpha_vector],[tau_alpha_array])      
      val VE = bindRel(VE,"vectorCopy",[tau_alpha_vector],[tau_alpha_vector])      

	  (* mutable arrays *)      
	  val VE = bindRel(VE,"arrayLength",[tau_alpha_array],[tau_int])
	  val VE = bindRel(VE,"arrayList",[tau_alpha_array],[tau_alpha_list])
	  val VE = bindRel(VE,"arrayNth",[tau_alpha_array,tau_int],[tau_alpha])
	  val VE = bindRel(VE,"arrayGet",[tau_alpha_array,tau_int],[tau_alpha])	  
	  val VE = bindRel(VE,"arraySetNth",[tau_alpha_array,tau_int,tau_alpha],[tau_alpha_array])
	  val VE = bindRel(VE,"arrayUpdate",[tau_alpha_array,tau_int,tau_alpha],[tau_alpha_array])	  
	  val VE = bindRel(VE,"arrayCreate",[tau_int, tau_alpha],[tau_alpha_array])
      val VE = bindRel(VE,"arrayAdd",[tau_alpha_array,tau_alpha],[tau_alpha_array])
      val VE = bindRel(VE,"arrayVector",[tau_alpha_array],[tau_alpha_vector])
      val VE = bindRel(VE,"arrayCopy",[tau_alpha_array],[tau_alpha_array])            
      	  
      (* if expressions *)
      val VE = bindRel(VE,"ifExp",[tau_bool,tau_alpha,tau_alpha],[tau_alpha])

	  (* misc *)
	  val VE = bindRel(VE,"clock",[],[tau_real])
	  val VE = bindRel(VE,"print",[tau_string],[])
	  val VE = bindRel(VE,"tick",[],[tau_int])
	  
	  (* debug *)
	  (* adrpo added *)
	  (* the debug buildin function takes 7 arguments
	   * file name
	   * sline, scolumn, eline, ecolumn
	   * relation
	   * goal as string
	   * values of the previous term (if exists) [32 tuples for now]
	   * parameters of the next term (if exists) [32 tuples for now]
	   *)
	  val VE = bindRel(VE,"debug", 
	            [
	             tau_string,		(* filename *) 
	             tau_int,			(* sline *)
	             tau_int,			(* scolumn *)
	             tau_int,			(* eline *)
	             tau_int,			(* ecolumn *)
	             tau_string,		(* relation *)
	             tau_string			(* goal *)
	            ],[])
	  val VE = bindRel(VE, "debug_print", tau_debug_param, [])
	  val VE = bindRel(VE, "debug_show_depth", [tau_int], [])
	  val VE = bindRel(VE, "debug_push_in01", makeParameterTyList([], 1), [])
	  val VE = bindRel(VE, "debug_push_in02", makeParameterTyList([], 2), [])
	  val VE = bindRel(VE, "debug_push_in03", makeParameterTyList([], 3), [])
	  val VE = bindRel(VE, "debug_push_in04", makeParameterTyList([], 4), [])	  
	  val VE = bindRel(VE, "debug_push_in05", makeParameterTyList([], 5), [])
	  val VE = bindRel(VE, "debug_push_in06", makeParameterTyList([], 6), [])
	  val VE = bindRel(VE, "debug_push_in07", makeParameterTyList([], 7), [])
	  val VE = bindRel(VE, "debug_push_in08", makeParameterTyList([], 8), [])
	  val VE = bindRel(VE, "debug_push_in09", makeParameterTyList([], 9), [])
	  val VE = bindRel(VE, "debug_push_in10", makeParameterTyList([],10), [])
	  val VE = bindRel(VE, "debug_push_in11", makeParameterTyList([],11), [])
	  val VE = bindRel(VE, "debug_push_in12", makeParameterTyList([],12), [])
	  val VE = bindRel(VE, "debug_push_in13", makeParameterTyList([],13), [])
	  val VE = bindRel(VE, "debug_push_in14", makeParameterTyList([],14), [])
	  val VE = bindRel(VE, "debug_push_in15", makeParameterTyList([],15), [])
	  val VE = bindRel(VE, "debug_push_in16", makeParameterTyList([],16), [])
	   
	  val VE = bindRel(VE, "debug_push_out01", tau_debug_param, [])
	  val VE = bindRel(VE, "debug_push_out02", makeParameterTyList([], 2), [])
	  val VE = bindRel(VE, "debug_push_out03", makeParameterTyList([], 3), [])
	  val VE = bindRel(VE, "debug_push_out04", makeParameterTyList([], 4), [])	  
	  val VE = bindRel(VE, "debug_push_out05", makeParameterTyList([], 5), [])
	  val VE = bindRel(VE, "debug_push_out06", makeParameterTyList([], 6), [])
	  val VE = bindRel(VE, "debug_push_out07", makeParameterTyList([], 7), [])
	  val VE = bindRel(VE, "debug_push_out08", makeParameterTyList([], 8), [])
	  val VE = bindRel(VE, "debug_push_out09", makeParameterTyList([], 9), [])
	  val VE = bindRel(VE, "debug_push_out10", makeParameterTyList([],10), [])
	  val VE = bindRel(VE, "debug_push_out11", makeParameterTyList([],11), [])
	  val VE = bindRel(VE, "debug_push_out12", makeParameterTyList([],12), [])
	  val VE = bindRel(VE, "debug_push_out13", makeParameterTyList([],13), [])
	  val VE = bindRel(VE, "debug_push_out14", makeParameterTyList([],14), [])
	  val VE = bindRel(VE, "debug_push_out15", makeParameterTyList([],15), [])
	  val VE = bindRel(VE, "debug_push_out16", makeParameterTyList([],16), [])
	  (* adrpo ----- *)
	  
      in
		VE
      end

    (* standard type environment *)
    val TE_init =
      let fun bind(TE, tycon, theta) =
	    let val tystr = TYSTR{theta=theta, abstract=false}
		val tycon = Absyn.rmlIdent tycon
	    in
	      IdentDict.insert(TE, tycon, tystr)
	    end
	  val TE = IdentDict.empty
	  val TE = bind(TE,"bool",TyFcn.lambda([],tau_bool))
	  val TE = bind(TE,"char",TyFcn.lambda([],tau_char))
	  val TE = bind(TE,"int",TyFcn.lambda([],tau_int))
	  val TE = bind(TE,"list",TyFcn.lambda([alpha],tau_alpha_list))
	  val TE = bind(TE,"lvar",TyFcn.lambda([alpha],tau_alpha_lvar))
	  val TE = bind(TE,"option",TyFcn.lambda([alpha],tau_alpha_option))
	  val TE = bind(TE,"real",TyFcn.lambda([],tau_real))
	  val TE = bind(TE,"string",TyFcn.lambda([],tau_string))
	  val TE = bind(TE,"vector",TyFcn.lambda([alpha],tau_alpha_vector))
	  val TE = bind(TE,"array",TyFcn.lambda([alpha],tau_alpha_array))	  
      in
		TE
      end

    (* standard module environment *)
    val ME_init =
      let val modstr = MODSTR{TE=TE_init, VE=VE_init, source=sourceInit}
	  val modid = Absyn.rmlIdent "RML"
      in
		IdentDict.insert(IdentDict.empty, modid, modstr)
      end

  end (* functor StatObjFn *)
