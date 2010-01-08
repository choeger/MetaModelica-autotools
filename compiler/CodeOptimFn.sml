(* code/code-optim.sml *)

functor CodeOptimFn(structure Util : UTIL
		    structure Code : CODE
		    structure CodeFVars : CODE_FVARS
		    structure Control :  CONTROL
		    sharing Code = CodeFVars.Code
		    sharing type Code.gvar = Code.gvar'
		      ) : CODE_OPTIM =
  struct

    structure Code = Code

    (*
     * This section performs Copy Propagation of global variables:
     * when a local variable is bound to a global one, this is recorded
     * in the copy context; when a global variable is assigned, aliases
     * depending on it are removed from the copy context. Should a
     * global variable be assigned to (a copy of) itself, then the
     * assignment is surpressed.
     *
     * The copy context must support the following operations:
     * 1. Recording that a local variable has been bound to a global variable.
     * 2. Looking up a local variable to see if it is bound to a global.
     * 3. Removing copies of a global variable when it is updated.
     * The copy context is currently implemented as a bag of (lvar,gvar) pairs.
     *)

    datatype Copy = COPY of {lvar: Code.lvar, gvar: Code.gvar}

    val copies_empty = []

    fun copies_bind_lvar(copies, lvar, gvar) = COPY{lvar=lvar,gvar=gvar} :: copies

    fun copies_lookup_lvar(copies, Code.LVAR{tag=tag1,...}) =	(* -> Code.GVar option *)
      let fun lookup([]) = NONE
	    | lookup(COPY{lvar=Code.LVAR{tag=tag2,...},gvar}::copies) =
		if tag1=tag2 then SOME gvar else lookup copies
      in
		lookup copies
      end

    fun copies_remove_gvar(copies_in, gvar') =
      let fun remove([], copies_out) = copies_out
	    | remove((copy as COPY{lvar,gvar})::copies_in, copies_out) =
		remove(copies_in, if gvar=gvar' then copies_out else copy::copies_out)
      in
		remove(copies_in, [])
      end

    fun propagate_value_copies(value, copies) =
      case value
		of Code.VAR(Code.GLOvar gvar)	=> SOME gvar
		 | Code.VAR(Code.LOCvar lvar)	=> copies_lookup_lvar(copies,lvar)
			 | _				=> NONE

    fun propagate_code_copies(code as Code.CODE{code=code',...}, copies) =
      case code'
	of Code.GOTO _ => code
	 | Code.STORE(dst, src, code)	=>
	    Code.mkSTORE(dst, src, propagate_code_copies(code,copies))
         | Code.BIND(NONE, rhs, code)	=>
	    Code.mkBIND(NONE, rhs, propagate_code_copies(code,copies))
	 | Code.BIND(lhs as SOME(Code.LOCvar lhs_lvar), rhs, code)	=>
	    let val copies = case propagate_value_copies(rhs,copies)
			       of NONE		=> copies
				| SOME rhs_gvar =>
				    copies_bind_lvar(copies,lhs_lvar,rhs_gvar)
	    in
	      Code.mkBIND(lhs, rhs, propagate_code_copies(code,copies))
	    end
	 | Code.BIND(lhs as SOME(Code.GLOvar lhs_gvar), rhs, code)	=>
	    let fun lose() =
		  let val copies = copies_remove_gvar(copies,lhs_gvar)
		  in
		    Code.mkBIND(lhs, rhs, propagate_code_copies(code,copies))
		  end
	    in
	      case propagate_value_copies(rhs,copies)
		of NONE		 => lose()
		 | SOME rhs_gvar =>
		    if lhs_gvar=rhs_gvar then propagate_code_copies(code,copies)
		    else lose()
	    end
	 | Code.SWITCH(value,cases,default)	=>
	    Code.mkSWITCH(value,
			  map (propagate_case_copies copies) cases,
			  propagateDefaultCopies(default, copies))

    and propagateDefaultCopies(NONE, _) = NONE
      | propagateDefaultCopies(SOME code, copies) =
	  SOME(propagate_code_copies(code, copies))

    and propagate_case_copies copies (tag,code) =
      (tag, propagate_code_copies(code,copies))

    fun propagate_labdef_copies(Code.LABDEF{globalP,label,varHP,nalloc,nargs,code, pos}) =
      let val code = propagate_code_copies(code, copies_empty)
      in
		Code.LABDEF{globalP=globalP, label=label, varHP=varHP,nalloc=nalloc, nargs=nargs, code=code, pos=pos}
      end

    fun propagate_module_copies(Code.MODULE{modname,ctors,xmods,xlabs,xvals,values,litdefs,labdefs,source}) =
      let val labdefs = map propagate_labdef_copies labdefs
      in
      	Code.MODULE{
      		modname=modname, ctors=ctors, xmods=xmods, xlabs=xlabs,
		    	xvals=xvals, values=values, litdefs=litdefs, labdefs=labdefs,
		    	source=source}
      end

    (*
     * This section performs Copy Elimination of local:=global bindings.
     * As a result of Copy Propagation, some aliases may become useless.
     * This section removes such bindings.
     * The code must be properly annotated with free variable information.
     *)

    fun code_uses_lvar(Code.CODE{fvars,...}, Code.LVAR{tag=tag1,...}) =
      List.exists (fn Code.LVAR{tag=tag2,...} => tag1=tag2) (!fvars)

    fun eliminate_code_copies(code as Code.CODE{code=code',...}) =
      case code'
	of Code.GOTO _ => code
	 | Code.STORE(dst, src, code)	=>
	     Code.mkSTORE(dst, src, eliminate_code_copies code)
	 | Code.BIND(NONE, rhs, code)	=>
	     Code.mkBIND(NONE, rhs, eliminate_code_copies code)
	 | Code.BIND(lhs as SOME(Code.LOCvar lvar), rhs, code)	=>
	     if code_uses_lvar(code,lvar)
	       then Code.mkBIND(lhs, rhs, eliminate_code_copies code)
	       else eliminate_code_copies code
	 | Code.BIND(lhs as SOME(Code.GLOvar _), rhs, code)	=>
	     Code.mkBIND(lhs, rhs, eliminate_code_copies code)
	 | Code.SWITCH(value,cases,default)	=>
	     Code.mkSWITCH(value,
			   map eliminate_case_copies cases,
			   eliminateDefaultCopies default)

    and eliminateDefaultCopies(NONE) = NONE
      | eliminateDefaultCopies(SOME code) = SOME(eliminate_code_copies code)

    and eliminate_case_copies(tag,code) = (tag, eliminate_code_copies code)

    fun eliminate_labdef_copies(Code.LABDEF{globalP,label,varHP,nalloc,nargs,code,pos}) =
      let val code = eliminate_code_copies code
      in
		Code.LABDEF{globalP=globalP, label=label, varHP=varHP, nalloc=nalloc, nargs=nargs, code=code, pos=pos}
      end

    fun eliminate_module_copies(Code.MODULE{modname,ctors,xmods,xlabs,xvals,values,litdefs,labdefs,source}) =
      let val labdefs = map eliminate_labdef_copies labdefs
      in
				Code.MODULE{
					modname=modname, ctors=ctors, xmods=xmods, xlabs=xlabs,
					xvals=xvals, values=values,litdefs=litdefs, labdefs=labdefs,
					source=source}
      end

    (* The optimizer first propagates copies of global variables, then
     * updates the free variable annotations, and finally eliminates
     * unnecessary copies.
     *)

    fun optimize module =
      let val module = propagate_module_copies module
	  val _ = CodeFVars.update module
	  val module = eliminate_module_copies module
      in
		module
      end

  end (* functor CodeOptimFn *)
