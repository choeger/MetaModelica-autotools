(* code/switch-intras.sml *)

functor SwitchIntrasFn(structure Util : UTIL
		       structure Code : SWITCH
		       sharing type Code.gvar = Code.gvar'
			 ) : SWITCH_INTRAS =
  struct

    structure Code = Code

    fun var_intras(var, intras) =
      case var
	of Code.GLOvar(Code.GVAR{scope,name})	=>
	    (case scope
	       of Code.INTRAgvs	=> Util.maybeCons(name, intras)
		| Code.INTERgvs	=> intras)
	 | Code.LOCvar _	=> intras

    fun bind_intras(NONE, intras)	= intras
      | bind_intras(SOME var, intras)	= var_intras(var, intras)

    fun val_intras(v, intras) =
      case v
	of Code.VAR var		=> var_intras(var, intras)
	 | Code.LITERAL _	=> intras
	 | Code.OFFSET(v, _)	=> val_intras(v, intras)
	 | Code.FETCH v		=> val_intras(v, intras)
	 | Code.UNTAGPTR v	=> val_intras(v, intras)
	 | Code.TAGPTR v	=> val_intras(v, intras)
	 | Code.CALL(_, vs)	=> List.foldl val_intras intras vs

    fun code_intras(Code.CODE{code,...}, intras) =
      case code
	of Code.GOTO(Code.LOCALg _,_,_,_,_) => intras
	 | Code.GOTO(Code.EXTERNg _,_,_,_,_) => intras
	 | Code.GOTO(Code.VALUEg v,_,_,_,_) => val_intras(v, intras)
	 | Code.STORE(v1, v2, c)	=>
	    code_intras(c, val_intras(v2, val_intras(v1, intras)))
	 | Code.BIND(binding, v, c)	=>
	    code_intras(c, val_intras(v, bind_intras(binding, intras)))
	 | Code.SWITCH(value,cases,default)	=>
	    List.foldl case_intras
		       (defaultIntras(default, val_intras(value, intras)))
		       cases

    and defaultIntras(NONE, intras) = intras
      | defaultIntras(SOME code, intras) = code_intras(code, intras)

    and case_intras((_,code), intras) = code_intras(code, intras)

    (* (Code.labdef * Code.gvar_name list) -> Code.gvar_name list *)
    fun labdefIntras(Code.LABDEF{code,...}, intras) = code_intras(code, intras)

  end (* functor SwitchIntrasFn *)
